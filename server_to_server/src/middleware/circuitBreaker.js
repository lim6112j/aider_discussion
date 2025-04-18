const config = require('../config/config');
const { logger } = require('../utils/logger');

/**
 * Circuit Breaker implementation
 */
class CircuitBreaker {
  constructor(options = {}) {
    this.failureThreshold = options.failureThreshold || config.circuitBreaker.failureThreshold;
    this.resetTimeout = options.resetTimeout || config.circuitBreaker.resetTimeout;
    this.state = 'CLOSED'; // CLOSED, OPEN, HALF_OPEN
    this.failureCount = 0;
    this.lastFailureTime = null;
    this.services = new Map();
  }

  /**
   * Get or create a circuit breaker for a specific service
   * @param {string} serviceName - The name of the service
   * @returns {CircuitBreaker} - The circuit breaker instance
   */
  static getBreaker(serviceName) {
    if (!CircuitBreaker.instances) {
      CircuitBreaker.instances = new Map();
    }

    if (!CircuitBreaker.instances.has(serviceName)) {
      CircuitBreaker.instances.set(serviceName, new CircuitBreaker());
    }

    return CircuitBreaker.instances.get(serviceName);
  }

  /**
   * Execute a function with circuit breaker protection
   * @param {Function} fn - The function to execute
   * @param {string} serviceName - The name of the service
   * @returns {Promise} - Result of the function
   */
  async execute(fn, serviceName) {
    if (!this.canRequest(serviceName)) {
      logger.warn(`Circuit is OPEN for service: ${serviceName}`);
      throw new Error(`Service ${serviceName} is unavailable (circuit open)`);
    }

    try {
      const result = await fn();
      this.onSuccess(serviceName);
      return result;
    } catch (error) {
      this.onFailure(serviceName);
      throw error;
    }
  }

  /**
   * Check if a request can be made
   * @param {string} serviceName - The name of the service
   * @returns {boolean} - Whether the request can be made
   */
  canRequest(serviceName) {
    const service = this.getServiceState(serviceName);
    
    if (service.state === 'CLOSED') {
      return true;
    }
    
    if (service.state === 'OPEN') {
      const timePassedSinceFailure = Date.now() - service.lastFailureTime;
      if (timePassedSinceFailure >= this.resetTimeout) {
        service.state = 'HALF_OPEN';
        logger.info(`Circuit changed to HALF_OPEN for service: ${serviceName}`);
        return true;
      }
      return false;
    }
    
    // HALF_OPEN state
    return true;
  }

  /**
   * Handle successful request
   * @param {string} serviceName - The name of the service
   */
  onSuccess(serviceName) {
    const service = this.getServiceState(serviceName);
    
    if (service.state === 'HALF_OPEN') {
      service.failureCount = 0;
      service.state = 'CLOSED';
      logger.info(`Circuit changed to CLOSED for service: ${serviceName}`);
    }
  }

  /**
   * Handle failed request
   * @param {string} serviceName - The name of the service
   */
  onFailure(serviceName) {
    const service = this.getServiceState(serviceName);
    
    service.failureCount += 1;
    service.lastFailureTime = Date.now();
    
    if (service.state === 'HALF_OPEN' || 
        (service.state === 'CLOSED' && service.failureCount >= this.failureThreshold)) {
      service.state = 'OPEN';
      logger.warn(`Circuit changed to OPEN for service: ${serviceName}`);
    }
  }

  /**
   * Get the state for a service
   * @param {string} serviceName - The name of the service
   * @returns {Object} - The service state
   */
  getServiceState(serviceName) {
    if (!this.services.has(serviceName)) {
      this.services.set(serviceName, {
        state: 'CLOSED',
        failureCount: 0,
        lastFailureTime: null
      });
    }
    
    return this.services.get(serviceName);
  }
}

/**
 * Circuit breaker middleware for Express
 * @param {string} serviceName - The name of the service
 * @returns {Function} - Express middleware
 */
function circuitBreakerMiddleware(serviceName) {
  return (req, res, next) => {
    const breaker = CircuitBreaker.getBreaker(serviceName);
    
    if (!breaker.canRequest(serviceName)) {
      logger.warn(`Circuit is OPEN for service: ${serviceName}`, { 
        correlationId: req.correlationId 
      });
      return res.status(503).json({ 
        error: 'Service Unavailable', 
        message: `Service ${serviceName} is temporarily unavailable` 
      });
    }
    
    // Track the original end method
    const originalEnd = res.end;
    
    res.end = function(...args) {
      if (res.statusCode >= 500) {
        breaker.onFailure(serviceName);
      } else {
        breaker.onSuccess(serviceName);
      }
      
      originalEnd.apply(res, args);
    };
    
    next();
  };
}

module.exports = {
  CircuitBreaker,
  circuitBreakerMiddleware
};
