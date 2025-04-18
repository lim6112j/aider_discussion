const { logger } = require('./logger');

/**
 * Implements exponential backoff retry logic
 * @param {Function} fn - The function to retry
 * @param {Object} options - Retry options
 * @param {number} options.initialDelay - Initial delay in ms
 * @param {number} options.maxDelay - Maximum delay in ms
 * @param {number} options.maxRetries - Maximum number of retries
 * @param {Function} options.shouldRetry - Function to determine if retry should happen
 * @returns {Promise} - Result of the function
 */
async function exponentialBackoff(fn, options = {}) {
  const {
    initialDelay = 1000,
    maxDelay = 60000,
    maxRetries = 5,
    shouldRetry = () => true,
  } = options;

  let retries = 0;
  let delay = initialDelay;

  while (true) {
    try {
      return await fn();
    } catch (error) {
      retries++;
      
      if (retries > maxRetries || !shouldRetry(error)) {
        logger.error(`Retry failed after ${retries} attempts`, { 
          error: error.message,
          retries
        });
        throw error;
      }

      logger.warn(`Retry attempt ${retries}/${maxRetries} after ${delay}ms`, {
        error: error.message,
        retries,
        delay
      });

      await new Promise(resolve => setTimeout(resolve, delay));
      
      // Exponential backoff with jitter
      delay = Math.min(maxDelay, delay * 2 * (0.9 + Math.random() * 0.2));
    }
  }
}

/**
 * Retry a function with a fixed delay
 * @param {Function} fn - The function to retry
 * @param {Object} options - Retry options
 * @returns {Promise} - Result of the function
 */
async function retryWithFixedDelay(fn, options = {}) {
  const {
    delay = 1000,
    maxRetries = 3,
    shouldRetry = () => true,
  } = options;

  let retries = 0;

  while (true) {
    try {
      return await fn();
    } catch (error) {
      retries++;
      
      if (retries > maxRetries || !shouldRetry(error)) {
        logger.error(`Retry failed after ${retries} attempts`, { 
          error: error.message,
          retries
        });
        throw error;
      }

      logger.warn(`Retry attempt ${retries}/${maxRetries} after ${delay}ms`, {
        error: error.message,
        retries,
        delay
      });

      await new Promise(resolve => setTimeout(resolve, delay));
    }
  }
}

module.exports = {
  exponentialBackoff,
  retryWithFixedDelay
};
