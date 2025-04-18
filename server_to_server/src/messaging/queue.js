const amqplib = require('amqplib');
const config = require('../config/config');
const { logger } = require('../utils/logger');
const { exponentialBackoff } = require('../utils/retry');

let connection = null;
let channel = null;

/**
 * Connect to RabbitMQ with retry logic
 */
async function connectToMessageQueue() {
  try {
    connection = await exponentialBackoff(
      async () => await amqplib.connect(config.rabbitmq.url),
      config.rabbitmq.retryOptions
    );
    
    connection.on('error', (err) => {
      logger.error('RabbitMQ connection error', { error: err.message });
      setTimeout(connectToMessageQueue, 5000);
    });
    
    connection.on('close', () => {
      logger.warn('RabbitMQ connection closed, attempting to reconnect');
      setTimeout(connectToMessageQueue, 5000);
    });
    
    channel = await connection.createChannel();
    
    // Setup queues and exchanges
    await setupQueuesAndExchanges();
    
    logger.info('Successfully connected to RabbitMQ');
    return { connection, channel };
  } catch (error) {
    logger.error('Failed to connect to RabbitMQ', { error: error.message });
    throw error;
  }
}

/**
 * Setup queues and exchanges
 */
async function setupQueuesAndExchanges() {
  // Create queues
  await channel.assertQueue(config.rabbitmq.queues.outbox, { durable: true });
  await channel.assertQueue(config.rabbitmq.queues.requests, { durable: true });
  await channel.assertQueue(config.rabbitmq.queues.responses, { durable: true });
  
  // Create exchanges
  await channel.assertExchange(config.rabbitmq.exchanges.events, 'topic', { durable: true });
  
  logger.info('Queues and exchanges set up successfully');
}

/**
 * Publish a message to a queue
 */
async function publishToQueue(queue, message, options = {}) {
  try {
    if (!channel) {
      await connectToMessageQueue();
    }
    
    const messageBuffer = Buffer.from(JSON.stringify(message));
    const publishOptions = {
      persistent: true,
      ...options,
    };
    
    return channel.sendToQueue(queue, messageBuffer, publishOptions);
  } catch (error) {
    logger.error('Failed to publish message to queue', { 
      queue, 
      error: error.message,
      correlationId: message.correlationId || 'unknown'
    });
    throw error;
  }
}

/**
 * Publish a message to an exchange
 */
async function publishToExchange(exchange, routingKey, message, options = {}) {
  try {
    if (!channel) {
      await connectToMessageQueue();
    }
    
    const messageBuffer = Buffer.from(JSON.stringify(message));
    const publishOptions = {
      persistent: true,
      ...options,
    };
    
    return channel.publish(exchange, routingKey, messageBuffer, publishOptions);
  } catch (error) {
    logger.error('Failed to publish message to exchange', { 
      exchange, 
      routingKey,
      error: error.message,
      correlationId: message.correlationId || 'unknown'
    });
    throw error;
  }
}

/**
 * Consume messages from a queue
 */
async function consumeFromQueue(queue, handler) {
  try {
    if (!channel) {
      await connectToMessageQueue();
    }
    
    return channel.consume(queue, async (msg) => {
      if (!msg) return;
      
      try {
        const content = JSON.parse(msg.content.toString());
        const correlationId = msg.properties.correlationId || 'unknown';
        
        logger.info(`Processing message from queue: ${queue}`, { correlationId });
        
        await handler(content, msg.properties);
        channel.ack(msg);
      } catch (error) {
        logger.error(`Error processing message from queue: ${queue}`, { 
          error: error.message,
          correlationId: msg.properties.correlationId || 'unknown'
        });
        
        // Negative acknowledgment with requeue
        channel.nack(msg, false, false);
      }
    });
  } catch (error) {
    logger.error(`Failed to consume from queue: ${queue}`, { error: error.message });
    throw error;
  }
}

/**
 * Close the connection
 */
async function closeConnection() {
  if (channel) {
    await channel.close();
  }
  if (connection) {
    await connection.close();
  }
  logger.info('Closed RabbitMQ connection');
}

module.exports = {
  connectToMessageQueue,
  publishToQueue,
  publishToExchange,
  consumeFromQueue,
  closeConnection,
};
