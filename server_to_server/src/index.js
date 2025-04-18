const express = require('express');
const { connectToMessageQueue } = require('./messaging/queue');
const routes = require('./api/routes');
const { logger } = require('./utils/logger');
const config = require('./config/config');
const mongoose = require('mongoose');

const app = express();

// Middleware
app.use(express.json());
app.use((req, res, next) => {
  req.correlationId = req.headers['x-correlation-id'] || Date.now().toString();
  logger.info(`Incoming request: ${req.method} ${req.url}`, { correlationId: req.correlationId });
  next();
});

// Health check endpoint
app.get('/health', (req, res) => {
  res.status(200).json({ status: 'ok', timestamp: new Date().toISOString() });
});

// API routes
app.use('/api', routes);

// Error handling middleware
app.use((err, req, res, next) => {
  logger.error('Unhandled error', { 
    error: err.message, 
    stack: err.stack,
    correlationId: req.correlationId 
  });
  res.status(500).json({ error: 'Internal server error' });
});

// Start server
async function startServer() {
  try {
    // Connect to MongoDB if configured
    if (config.mongodb.uri) {
      await mongoose.connect(config.mongodb.uri);
      logger.info('Connected to MongoDB');
    }
    
    // Connect to message queue
    await connectToMessageQueue();
    logger.info('Connected to message queue');
    
    // Start Express server
    const PORT = config.server.port || 3000;
    app.listen(PORT, () => {
      logger.info(`Server running on port ${PORT}`);
    });
  } catch (error) {
    logger.error('Failed to start server', { error: error.message, stack: error.stack });
    process.exit(1);
  }
}

startServer();

// Handle graceful shutdown
process.on('SIGTERM', () => {
  logger.info('SIGTERM received, shutting down gracefully');
  // Close server, DB connections, etc.
  process.exit(0);
});

module.exports = app; // For testing
