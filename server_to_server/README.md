# Server-to-Server Communication Project

A robust implementation of server-to-server communication with resilience patterns for handling network interruptions, retries, and ensuring data consistency.

## Features

- **Circuit Breaker Pattern**: Prevents cascading failures when downstream services are unavailable
- **Retry Mechanisms**: Exponential backoff for transient failures
- **Message Queue Integration**: Reliable asynchronous communication
- **Idempotency Support**: Safe retries without side effects
- **Outbox Pattern**: Ensures reliable message delivery
- **Correlation IDs**: Track requests across services
- **Structured Logging**: Consistent log format with correlation tracking
- **Health Checks**: Monitor service health
- **Containerization**: Docker and Docker Compose setup

## Project Structure

```
server_to_server/
├── src/
│   ├── api/                  # API definitions and handlers
│   ├── config/               # Configuration management
│   ├── messaging/            # Message queue integration
│   ├── models/               # Data models
│   ├── services/             # Business logic
│   ├── utils/                # Utility functions
│   └── middleware/           # Custom middleware
├── tests/                    # Test files
├── package.json              # Dependencies and scripts
├── docker-compose.yml        # Service definitions
├── Dockerfile                # Container definition
└── README.md                 # Project documentation
```

## Getting Started

### Prerequisites

- Node.js 14+
- Docker and Docker Compose
- RabbitMQ
- Redis
- MongoDB

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/server-to-server.git
   cd server-to-server
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

3. Create a `.env` file:
   ```
   NODE_ENV=development
   PORT=3000
   RABBITMQ_URL=amqp://localhost:5672
   REDIS_URL=redis://localhost:6379
   MONGODB_URI=mongodb://localhost:27017/server_to_server
   LOG_LEVEL=info
   ```

### Running with Docker

```bash
docker-compose up -d
```

### Running Locally

```bash
npm run dev
```

## API Endpoints

- `GET /health`: Health check endpoint
- `POST /api/service-a/resource`: Send a request to Service A
- `GET /api/service-b/resource/:id`: Get a resource from Service B
- `POST /api/webhooks/:service`: Webhook endpoint for receiving events

## Resilience Patterns

### Circuit Breaker

The circuit breaker prevents cascading failures by stopping requests to failing services. It has three states:
- **CLOSED**: Normal operation, requests pass through
- **OPEN**: Service is failing, requests are blocked
- **HALF-OPEN**: Testing if service has recovered

### Retry with Exponential Backoff

Automatically retries failed requests with increasing delays to avoid overwhelming the target service.

### Outbox Pattern

Ensures reliable message delivery by storing messages in a database before sending them to the message queue.

### Idempotency

Prevents duplicate processing of requests by using idempotency keys.

## Testing

```bash
npm test
```

## License

ISC
