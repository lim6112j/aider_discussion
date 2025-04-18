const Redis = require('ioredis');
const config = require('../config/config');
const { logger } = require('../utils/logger');

// Create Redis client
const redis = new Redis(config.redis.url);

/**
 * Middleware to ensure idempotent requests
 * @param {Object} options - Middleware options
 * @returns {Function} - Express middleware
 */
function idempotencyMiddleware(options = {}) {
  const {
    headerName = 'x-idempotency-key',
    ttl = config.idempotency.keyTTL,
    responseHeader = 'x-idempotency-replayed'
  } = options;

  return async (req, res, next) => {
    const idempotencyKey = req.headers[headerName];
    const correlationId = req.correlationId;

    // If no idempotency key is provided, proceed without idempotency check
    if (!idempotencyKey) {
      logger.warn('No idempotency key provided', { correlationId });
      return next();
    }

    // Create a Redis key that includes the request method and path
    const redisKey = `idempotency:${req.method}:${req.path}:${idempotencyKey}`;

    try {
      // Check if we've seen this request before
      const existingResponse = await redis.get(redisKey);

      if (existingResponse) {
        // Request has been processed before, return the cached response
        logger.info('Returning cached idempotent response', { 
          idempotencyKey,
          correlationId
        });

        const parsedResponse = JSON.parse(existingResponse);
        
        // Set header to indicate this is a replayed response
        res.set(responseHeader, 'true');
        
        // Return the cached response
        return res.status(parsedResponse.status)
          .set(parsedResponse.headers)
          .send(parsedResponse.body);
      }

      // Capture the original response methods
      const originalSend = res.send;
      const originalJson = res.json;
      const originalStatus = res.status;
      
      let responseBody;
      let responseStatus = 200;
      const responseHeaders = {};

      // Override response methods to capture the response
      res.send = function(body) {
        responseBody = body;
        return originalSend.call(this, body);
      };

      res.json = function(body) {
        responseBody = JSON.stringify(body);
        return originalJson.call(this, body);
      };

      res.status = function(code) {
        responseStatus = code;
        return originalStatus.call(this, code);
      };

      // Capture response headers
      const originalSetHeader = res.setHeader;
      res.setHeader = function(name, value) {
        responseHeaders[name] = value;
        return originalSetHeader.call(this, name, value);
      };

      // Add a listener for when the response is finished
      res.on('finish', async () => {
        // Only cache successful responses
        if (responseStatus >= 200 && responseStatus < 500) {
          try {
            const responseData = {
              status: responseStatus,
              headers: responseHeaders,
              body: responseBody
            };

            // Store the response in Redis with expiration
            await redis.set(redisKey, JSON.stringify(responseData), 'EX', ttl);
            
            logger.info('Cached idempotent response', { 
              idempotencyKey,
              correlationId,
              ttl
            });
          } catch (error) {
            logger.error('Failed to cache idempotent response', { 
              error: error.message,
              idempotencyKey,
              correlationId
            });
          }
        }
      });

      next();
    } catch (error) {
      logger.error('Error in idempotency middleware', { 
        error: error.message,
        idempotencyKey,
        correlationId
      });
      next(error);
    }
  };
}

module.exports = {
  idempotencyMiddleware
};
