const winston = require('winston');
const config = require('../config/config');

// Define log format
const logFormat = winston.format.combine(
  winston.format.timestamp(),
  winston.format.errors({ stack: true }),
  winston.format.json()
);

// Create logger instance
const logger = winston.createLogger({
  level: config.logging.level,
  format: logFormat,
  defaultMeta: { service: 'server-to-server' },
  transports: [
    new winston.transports.Console({
      format: winston.format.combine(
        winston.format.colorize(),
        winston.format.printf(({ timestamp, level, message, correlationId, ...meta }) => {
          const correlationInfo = correlationId ? `[${correlationId}]` : '';
          const metaInfo = Object.keys(meta).length ? JSON.stringify(meta) : '';
          return `${timestamp} ${level}: ${correlationInfo} ${message} ${metaInfo}`;
        })
      )
    }),
    // Add file transport for non-development environments
    ...(config.server.env !== 'development' ? [
      new winston.transports.File({ 
        filename: 'logs/error.log', 
        level: 'error',
        maxsize: 10485760, // 10MB
        maxFiles: 5
      }),
      new winston.transports.File({ 
        filename: 'logs/combined.log',
        maxsize: 10485760, // 10MB
        maxFiles: 5
      })
    ] : [])
  ]
});

// Create a child logger with correlation ID
function createChildLogger(correlationId) {
  return logger.child({ correlationId });
}

module.exports = {
  logger,
  createChildLogger
};
