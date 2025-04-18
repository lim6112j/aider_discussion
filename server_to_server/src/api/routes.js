const express = require('express');
const { circuitBreakerMiddleware } = require('../middleware/circuitBreaker');
const { idempotencyMiddleware } = require('../middleware/idempotency');
const { logger } = require('../utils/logger');
const { makeRequest } = require('../services/requestService');
const { createOutboxMessage } = require('../services/outboxService');

const router = express.Router();

// Apply idempotency middleware to all POST/PUT/PATCH requests
router.use((req, res, next) => {
  if (['POST', 'PUT', 'PATCH'].includes(req.method)) {
    return idempotencyMiddleware()(req, res, next);
  }
  next();
});

// Example endpoint that communicates with Service A
router.post('/service-a/resource', 
  circuitBreakerMiddleware('serviceA'),
  async (req, res) => {
    try {
      const { body } = req;
      const correlationId = req.correlationId;
      
      logger.info('Processing request to Service A', { correlationId });
      
      // Create an outbox message for reliable delivery
      await createOutboxMessage({
        service: 'serviceA',
        action: 'createResource',
        payload: body,
        correlationId
      });
      
      // Return accepted response immediately
      res.status(202).json({ 
        message: 'Request accepted for processing',
        correlationId
      });
    } catch (error) {
      logger.error('Error processing request to Service A', { 
        error: error.message,
        correlationId: req.correlationId
      });
      
      res.status(500).json({ error: 'Failed to process request' });
    }
  }
);

// Example endpoint that makes a synchronous request to Service B
router.get('/service-b/resource/:id', 
  circuitBreakerMiddleware('serviceB'),
  async (req, res) => {
    try {
      const { id } = req.params;
      const correlationId = req.correlationId;
      
      logger.info(`Fetching resource ${id} from Service B`, { correlationId });
      
      const response = await makeRequest({
        service: 'serviceB',
        method: 'GET',
        path: `/api/resources/${id}`,
        correlationId
      });
      
      res.status(200).json(response.data);
    } catch (error) {
      logger.error(`Error fetching resource from Service B`, { 
        error: error.message,
        correlationId: req.correlationId
      });
      
      // Determine appropriate status code based on error
      const statusCode = error.response?.status || 500;
      const errorMessage = error.response?.data?.error || 'Failed to fetch resource';
      
      res.status(statusCode).json({ error: errorMessage });
    }
  }
);

// Webhook endpoint to receive events from other services
router.post('/webhooks/:service', 
  idempotencyMiddleware(),
  async (req, res) => {
    try {
      const { service } = req.params;
      const { body } = req;
      const correlationId = req.correlationId;
      
      logger.info(`Received webhook from ${service}`, { correlationId });
      
      // Process the webhook asynchronously
      createOutboxMessage({
        service: 'webhook',
        action: 'processWebhook',
        payload: {
          service,
          data: body
        },
        correlationId
      });
      
      // Acknowledge receipt immediately
      res.status(200).json({ received: true });
    } catch (error) {
      logger.error('Error processing webhook', { 
        error: error.message,
        correlationId: req.correlationId
      });
      
      res.status(500).json({ error: 'Failed to process webhook' });
    }
  }
);

module.exports = router;
