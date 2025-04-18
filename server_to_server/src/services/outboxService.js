const mongoose = require('mongoose');
const { publishToQueue } = require('../messaging/queue');
const config = require('../config/config');
const { logger } = require('../utils/logger');
const { exponentialBackoff } = require('../utils/retry');

// Define Outbox Message Schema
const OutboxMessageSchema = new mongoose.Schema({
  service: {
    type: String,
    required: true,
    index: true
  },
  action: {
    type: String,
    required: true
  },
  payload: {
    type: mongoose.Schema.Types.Mixed,
    required: true
  },
  correlationId: {
    type: String,
    required: true,
    index: true
  },
  status: {
    type: String,
    enum: ['pending', 'processing', 'completed', 'failed'],
    default: 'pending',
    index: true
  },
  retryCount: {
    type: Number,
    default: 0
  },
  error: String,
  createdAt: {
    type: Date,
    default: Date.now,
    index: true
  },
  processedAt: Date
});

const OutboxMessage = mongoose.model('OutboxMessage', OutboxMessageSchema);

/**
 * Create a new outbox message
 * @param {Object} messageData - The message data
 * @returns {Promise<Object>} - The created message
 */
async function createOutboxMessage(messageData) {
  try {
    const message = new OutboxMessage({
      service: messageData.service,
      action: messageData.action,
      payload: messageData.payload,
      correlationId: messageData.correlationId
    });
    
    await message.save();
    
    logger.info('Created outbox message', { 
      messageId: message._id,
      correlationId: messageData.correlationId
    });
    
    // Trigger processing of the message
    await publishToQueue(config.rabbitmq.queues.outbox, {
      messageId: message._id.toString(),
      correlationId: messageData.correlationId
    });
    
    return message;
  } catch (error) {
    logger.error('Failed to create outbox message', { 
      error: error.message,
      correlationId: messageData.correlationId
    });
    throw error;
  }
}

/**
 * Process outbox messages
 * @param {Object} message - The message from the queue
 */
async function processOutboxMessage(message) {
  const { messageId, correlationId } = message;
  
  try {
    // Find and lock the message
    const outboxMessage = await OutboxMessage.findOneAndUpdate(
      { _id: messageId, status: { $in: ['pending', 'failed'] } },
      { status: 'processing' },
      { new: true }
    );
    
    if (!outboxMessage) {
      logger.warn('Outbox message not found or already processed', { 
        messageId,
        correlationId
      });
      return;
    }
    
    logger.info('Processing outbox message', { 
      messageId,
      service: outboxMessage.service,
      action: outboxMessage.action,
      correlationId
    });
    
    // Process the message based on service and action
    await exponentialBackoff(
      async () => {
        switch (outboxMessage.service) {
          case 'serviceA':
            await processServiceAMessage(outboxMessage);
            break;
          case 'serviceB':
            await processServiceBMessage(outboxMessage);
            break;
          case 'webhook':
            await processWebhookMessage(outboxMessage);
            break;
          default:
            throw new Error(`Unknown service: ${outboxMessage.service}`);
        }
      },
      {
        initialDelay: 1000,
        maxDelay: 30000,
        maxRetries: 5,
        shouldRetry: (error) => {
          // Don't retry certain errors
          return !error.message.includes('validation failed');
        }
      }
    );
    
    // Mark as completed
    await OutboxMessage.findByIdAndUpdate(
      messageId,
      { 
        status: 'completed',
        processedAt: new Date()
      }
    );
    
    logger.info('Successfully processed outbox message', { 
      messageId,
      correlationId
    });
  } catch (error) {
    logger.error('Failed to process outbox message', { 
      messageId,
      error: error.message,
      correlationId
    });
    
    // Update message with failure information
    await OutboxMessage.findByIdAndUpdate(
      messageId,
      { 
        status: 'failed',
        retryCount: { $inc: 1 },
        error: error.message
      }
    );
    
    // Requeue for retry if under max retries
    const outboxMessage = await OutboxMessage.findById(messageId);
    if (outboxMessage && outboxMessage.retryCount < 5) {
      setTimeout(async () => {
        await publishToQueue(config.rabbitmq.queues.outbox, {
          messageId,
          correlationId
        });
      }, 5000 * Math.pow(2, outboxMessage.retryCount)); // Exponential backoff
    }
  }
}

// Implement service-specific message processors
async function processServiceAMessage(message) {
  // Implementation for Service A
  logger.info(`Processing Service A message: ${message.action}`);
  // Add your implementation here
}

async function processServiceBMessage(message) {
  // Implementation for Service B
  logger.info(`Processing Service B message: ${message.action}`);
  // Add your implementation here
}

async function processWebhookMessage(message) {
  // Implementation for webhooks
  logger.info(`Processing webhook: ${message.payload.service}`);
  // Add your implementation here
}

/**
 * Start the outbox processor
 */
function startOutboxProcessor() {
  const { consumeFromQueue } = require('../messaging/queue');
  
  consumeFromQueue(config.rabbitmq.queues.outbox, processOutboxMessage)
    .then(() => {
      logger.info('Outbox processor started');
    })
    .catch((error) => {
      logger.error('Failed to start outbox processor', { error: error.message });
    });
}

module.exports = {
  createOutboxMessage,
  processOutboxMessage,
  startOutboxProcessor
};
