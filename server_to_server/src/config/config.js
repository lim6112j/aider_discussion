require('dotenv').config();

const config = {
  server: {
    port: process.env.PORT || 3000,
    env: process.env.NODE_ENV || 'development',
  },
  rabbitmq: {
    url: process.env.RABBITMQ_URL || 'amqp://localhost:5672',
    retryOptions: {
      initialDelay: 1000,
      maxDelay: 60000,
      maxRetries: 10,
    },
    queues: {
      outbox: 'outbox_queue',
      requests: 'requests_queue',
      responses: 'responses_queue',
    },
    exchanges: {
      events: 'events_exchange',
    },
  },
  redis: {
    url: process.env.REDIS_URL || 'redis://localhost:6379',
    ttl: 3600, // 1 hour in seconds
  },
  mongodb: {
    uri: process.env.MONGODB_URI || 'mongodb://localhost:27017/server_to_server',
  },
  circuitBreaker: {
    failureThreshold: 5,
    resetTimeout: 30000, // 30 seconds
  },
  idempotency: {
    keyTTL: 86400, // 24 hours in seconds
  },
  logging: {
    level: process.env.LOG_LEVEL || 'info',
  },
  externalServices: {
    serviceA: {
      baseUrl: process.env.SERVICE_A_URL || 'http://service-a:3000',
      timeout: 5000, // 5 seconds
    },
    serviceB: {
      baseUrl: process.env.SERVICE_B_URL || 'http://service-b:3000',
      timeout: 5000,
    },
  },
};

module.exports = config;
