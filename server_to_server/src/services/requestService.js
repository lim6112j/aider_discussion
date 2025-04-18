const axios = require('axios');
const { CircuitBreaker } = require('../middleware/circuitBreaker');
const config = require('../config/config');
const { logger } = require('../utils/logger');
const { exponentialBackoff } = require('../utils/retry');

/**
 * Make a request to another service with retry and circuit breaker
 * @param {Object} options - Request options
 * @returns {Promise<Object>} - Response from the service
 */
async function makeRequest(options) {
  const {
    service,
    method = 'GET',
    path,
    data = null,
    headers = {},
    timeout = 5000,
    correlationId
  } = options;

  // Get service configuration
  const serviceConfig = config.externalServices[service];
  if (!serviceConfig) {
    throw new Error(`Service configuration not found for: ${service}`);
  }

  // Get circuit breaker for this service
  const breaker = CircuitBreaker.getBreaker(service);

  // Create request config
  const requestConfig = {
    method,
    url: `${serviceConfig.baseUrl}${path}`,
    headers: {
      'Content-Type': 'application/json',
      'x-correlation-id': correlationId,
      ...headers
    },
    timeout: timeout || serviceConfig.timeout,
    data: method !== 'GET' ? data : undefined,
    params: method === 'GET' && data ? data : undefined
  };

  // Execute request with circuit breaker
  return breaker.execute(async () => {
    try {
      logger.info(`Making ${method} request to ${service}: ${path}`, { correlationId });
      
      // Use exponential backoff for retries
      const response = await exponentialBackoff(
        async () => axios(requestConfig),
        {
          initialDelay: 1000,
          maxDelay: 10000,
          maxRetries: 3,
          shouldRetry: (error) => {
            // Only retry on network errors or 5xx responses
            return !error.response || error.response.status >= 500;
          }
        }
      );
      
      logger.info(`Received response from ${service}: ${response.status}`, { correlationId });
      return response;
    } catch (error) {
      logger.error(`Request to ${service} failed`, { 
        error: error.message,
        status: error.response?.status,
        correlationId
      });
      throw error;
    }
  }, service);
}

module.exports = {
  makeRequest
};
