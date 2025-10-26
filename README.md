# R Stats Microservice

[![CI](https://github.com/MortalityWatch/rstats.mortality.watch/actions/workflows/ci.yml/badge.svg)](https://github.com/MortalityWatch/rstats.mortality.watch/actions/workflows/ci.yml)

Statistical forecasting API for mortality data baseline calculations.

## Overview

This microservice provides HTTP endpoints for time series forecasting and statistical baseline calculations. It's designed as a companion service to the MortalityWatch platform, handling computationally intensive statistical operations that are best performed in R.

### Features

- **Multiple Forecasting Methods**: Naive, mean, linear regression, exponential smoothing
- **Seasonality Support**: Annual, quarterly, monthly, weekly data
- **Cumulative Forecasting**: Special handling for cumulative annual data
- **Response Caching**: 1-hour TTL for identical requests
- **Rate Limiting**: 100 requests per minute per IP
- **Health Monitoring**: `/health` endpoint for service monitoring
- **Structured Logging**: Detailed request tracking with timestamps
- **Input Validation**: Comprehensive parameter checking with descriptive errors

## Technology Stack

- **R 4.x** - Statistical computing
- **Fiery** - Lightweight HTTP server
- **fable** - Time series forecasting
- **tsibble** - Time series data structures
- **tidyverse** - Data manipulation
- **Docker** - Containerization (r2u:22.04 base image)

## Quick Start

### Local Development

```bash
# Install R dependencies
./install_r_deps.sh

# Start server
Rscript src/serve.r

# Server runs on http://localhost:5000
```

### Docker

```bash
# Build image
docker build -t rstats-service .

# Run container
docker run -p 5000:5000 rstats-service
```

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `PORT` | `5000` | HTTP server port |
| `ALLOWED_ORIGINS` | `https://www.mortality.watch,https://mortality.watch` | Comma-separated list of allowed CORS origins |

## API Documentation

### Endpoints

#### `GET /health`

Health check endpoint for monitoring.

**Response:**
```json
{
  "status": "ok",
  "timestamp": "2025-10-26T12:00:00Z",
  "version": "1.0.0"
}
```

#### `GET /`

Standard time series forecast with seasonality.

**Parameters:**
- `y` (required): Comma-separated numeric values (min 3, max 10000)
- `h` (required): Forecast horizon (1-1000)
- `s` (required): Seasonality type
  - `1` = Annual
  - `2` = Quarterly
  - `3` = Monthly
  - `4` = Weekly
- `m` (required): Forecasting method
  - `naive` = Naive forecast
  - `mean` = Seasonal mean
  - `lin_reg` = Linear regression
  - `exp` = Exponential smoothing (ETS)
- `t` (optional): Include trend (0=no, 1=yes, default=0)

**Example Request:**
```bash
curl "http://localhost:5000/?y=100,105,110,108,112,115,120&h=3&s=1&m=lin_reg&t=1"
```

**Response:**
```json
{
  "y": [100.0, 105.2, 109.8, 108.1, 112.3, 115.0, 119.9, 124.5, 129.1, 133.7],
  "lower": [null, null, null, null, null, null, null, 120.2, 124.5, 128.8],
  "upper": [null, null, null, null, null, null, null, 128.8, 133.7, 138.6]
}
```

#### `GET /cum`

Cumulative forecast for annual data.

**Parameters:**
- `y` (required): Comma-separated cumulative values (min 3, max 10000)
- `h` (required): Forecast horizon (1-1000)
- `t` (optional): Include trend (0=no, 1=yes, default=0)

**Example Request:**
```bash
curl "http://localhost:5000/cum?y=1000,2100,3300,4600&h=2&t=1"
```

**Response:**
```json
{
  "y": [1000.0, 1100.0, 1200.0, 1300.0, 1400.0, 1500.0],
  "lower": [null, null, null, null, 1350.2, 1450.3],
  "upper": [null, null, null, null, 1449.8, 1549.7]
}
```

### Response Headers

- `X-Cache`: Either `HIT` (served from cache) or `MISS` (computed)
- `Content-Type`: `application/json`

### Error Responses

All errors return JSON with `error` and `status` fields:

```json
{
  "error": "Parameter 'y' must contain at least 3 valid data points",
  "status": 400
}
```

**Common Error Codes:**
- `400` - Bad Request (validation failed)
- `404` - Not Found (invalid route)
- `429` - Too Many Requests (rate limit exceeded)
- `500` - Internal Server Error (processing failed)

## Performance Features

### Caching

Responses are cached in memory for 1 hour (3600 seconds). Identical requests return cached results immediately:

```
First request:  X-Cache: MISS  (500ms)
Second request: X-Cache: HIT   (2ms)
```

Cache TTL can be modified in `src/serve.r`:
```r
CACHE_TTL <- 3600  # seconds
```

### Rate Limiting

100 requests per minute per IP address. Exceeding this returns HTTP 429:

```json
{
  "error": "Rate limit exceeded. Maximum 100 requests per minute.",
  "status": 429
}
```

Configuration in `src/serve.r`:
```r
RATE_LIMIT_WINDOW <- 60         # seconds
RATE_LIMIT_MAX_REQUESTS <- 100  # requests
```

## Security Features

### CORS (Cross-Origin Resource Sharing)

The service implements strict CORS policies to only allow requests from whitelisted origins.

**Default allowed origins:**
- `https://www.mortality.watch`
- `https://mortality.watch`

**How it works:**
- Requests from non-whitelisted origins receive HTTP 403
- Preflight OPTIONS requests are handled correctly
- CORS headers are only set for allowed origins
- Requests without an Origin header (e.g., curl, Postman) are allowed

**Custom configuration:**
```bash
# Allow additional origins
export ALLOWED_ORIGINS="https://www.mortality.watch,https://mortality.watch,https://dev.mortality.watch"
```

**CORS rejection response:**
```json
{
  "error": "Origin not allowed",
  "status": 403
}
```

**Logs:**
```
[2025-10-26 12:35:15] WARN: CORS rejected - ip=192.168.1.3, origin=https://evil.com
```

## Project Structure

```
.
├── src/
│   ├── serve.r           # Main server and routing
│   ├── handlers.r        # Forecast request handlers
│   └── utils.r           # Statistical utility functions
├── tests/
│   ├── test_utils.r      # Tests for utility functions
│   ├── test_validation.r # Tests for validation/caching
│   └── run_tests.r       # Test runner
├── dependencies_r.txt    # R package dependencies
├── dependencies.txt      # System dependencies
├── install_r_deps.sh     # Dependency installer
├── Dockerfile            # Container configuration
└── README.md            # This file
```

## Testing

Run the test suite:

```bash
cd tests
./run_tests.r
```

Tests cover:
- Statistical utility functions
- Input validation
- Caching logic
- Rate limiting
- Error handling

## Deployment

### Dokku

The service is configured for Dokku deployment with nginx plugins:

```bash
# Deploy
git push dokku master

# Enable CORS
dokku nginx-cors:enable rstats-mortality-watch

# Enable caching
dokku nginx-cache:enable rstats-mortality-watch
```

**Port Configuration:**
- The service runs on port 5000 internally (configurable via `PORT` env var)
- Dokku automatically proxies external port 80 to internal port 5000
- No explicit port mapping needed in `deployments/config.json`

Pre-deployment script: `pre-deploy.sh`

### Docker Compose

```yaml
version: '3.8'
services:
  rstats:
    build: .
    ports:
      - "5000:5000"
    environment:
      - PORT=5000
    restart: unless-stopped
```

### Health Checks

Configure container health checks:

```dockerfile
HEALTHCHECK --interval=30s --timeout=3s --retries=3 \
  CMD curl -f http://localhost:5000/health || exit 1
```

## Logging

Structured logs with timestamp, level, and details:

```
[2025-10-26 12:34:56] INFO: Starting server - port=5000
[2025-10-26 12:35:01] INFO: Incoming request - path=/, ip=192.168.1.1, params=y=100,105,...
[2025-10-26 12:35:02] INFO: Request completed - path=/, ip=192.168.1.1, duration_ms=487.23, status=200, cached=yes
[2025-10-26 12:35:05] WARN: Rate limit exceeded - ip=192.168.1.2
[2025-10-26 12:35:10] ERROR: Processing failed - path=/, error=insufficient data points
```

## Statistical Methods

### Naive Forecast
Simple projection using the most recent observation.

### Mean Forecast
Seasonal average with optional seasonal components.

### Linear Regression
Trend and/or seasonal linear model using TSLM.

### Exponential Smoothing (ETS)
Automated exponential smoothing with error, trend, and seasonal components.

### Cumulative Forecast
Special handling for cumulative annual data with proper uncertainty intervals.

## Development

### Adding New Methods

1. Add handler to `src/handlers.r`
2. Add validation to `src/serve.r` in `validate_request()`
3. Update routing in main request handler
4. Add tests to `tests/`
5. Update this README

### Code Style

- Document functions with roxygen2-style comments
- Use descriptive variable names
- Follow tidyverse style guide
- Add error handling for all external inputs

## License

AGPL-3.0 - see [LICENSE](LICENSE) file for details.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

## Support

- **Issues**: Report bugs or request features via GitHub issues
- **Documentation**: See comments in source files for detailed function documentation

## Changelog

### v1.0.0 (2025-10-26)

**Major Refactor:**
- Split monolithic serve.r into modular components
- Added health check endpoint
- Implemented request validation
- Added rate limiting
- Added response caching
- Structured logging with request tracking
- Comprehensive test suite
- Improved error handling

**Breaking Changes:**
- None (API remains backwards compatible)

**Bug Fixes:**
- Added missing tsibble dependency
- Removed unused Node.js dependencies

---

Built with ❤️ using R and Fiery
