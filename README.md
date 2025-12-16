# R Stats Microservice

[![CI](https://github.com/MortalityWatch/stats.mortality.watch/actions/workflows/ci.yml/badge.svg)](https://github.com/MortalityWatch/stats.mortality.watch/actions/workflows/ci.yml)

Statistical forecasting API for mortality data baseline calculations.

## Overview

This microservice provides HTTP endpoints for time series forecasting and statistical baseline calculations. It's designed as a companion service to the MortalityWatch platform, handling computationally intensive statistical operations that are best performed in R.

### Features

- **Multiple Forecasting Methods**: Naive, mean, linear regression, exponential smoothing
- **Life Table Calculation**: Life expectancy (e₀, e₆₅) via Chiang's method (pure R implementation)
- **STL Decomposition**: Seasonal-trend decomposition for sub-yearly life expectancy data
- **Seasonality Support**: Annual, quarterly, monthly, weekly data
- **Cumulative Forecasting**: Special handling for cumulative annual data
- **Response Caching**: 1-hour TTL for identical requests
- **Rate Limiting**: 100 requests per minute per IP
- **Health Monitoring**: `/health` endpoint for service monitoring
- **Structured Logging**: Detailed request tracking with timestamps
- **Input Validation**: Comprehensive parameter checking with descriptive errors
- **Error Tracking**: Sentry integration for production error monitoring

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
docker build -t stats-service .

# Run container
docker run -p 5000:5000 stats-service
```

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `PORT` | `5000` | HTTP server port |
| `ALLOWED_ORIGINS` | `https://www.mortality.watch,https://mortality.watch` | Comma-separated list of allowed CORS origins |
| `SENTRY_DSN` | _(empty)_ | Sentry DSN for error tracking (optional) |
| `SENTRY_ENVIRONMENT` | `production` | Sentry environment name |
| `SENTRY_TRACES_SAMPLE_RATE` | `0.1` | Sentry performance monitoring sample rate (0.0-1.0) |

**Note**: Copy `.env.example` to `.env` and configure as needed for local development.

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
  "upper": [null, null, null, null, null, null, null, 128.8, 133.7, 138.6],
  "zscore": [-0.15, 0.12, -0.05, -0.18, 0.08, -0.02, 0.20, 0.0, 0.0, 0.0]
}
```

**Response Fields:**
- `y`: Fitted baseline values (for historical period) + forecast values (for future period). Rounded to 1 decimal place.
- `lower`: 95% prediction interval lower bound (only for forecast period). Rounded to 1 decimal place.
- `upper`: 95% prediction interval upper bound (only for forecast period). Rounded to 1 decimal place.
- `zscore`: Standardized residuals showing how many standard deviations each observed value is from the fitted baseline. Rounded to 3 decimal places to match frontend's maximum precision setting. Values beyond ±2 are statistically significant (95% CI). Forecast period values are 0 (by definition).

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
  "upper": [null, null, null, null, 1449.8, 1549.7],
  "zscore": [-0.08, 0.15, -0.05, 0.12, 0.0, 0.0]
}
```

#### `GET /lt`

Life table calculation for life expectancy estimation using Chiang's method.

**Parameters:**
- `deaths` (required): Deaths by age group
  - Single period: comma-separated (e.g., `500,1000,5000,10000`)
  - Multiple periods: semicolon-separated rows (e.g., `500,1000;510,1020;490,980`)
- `population` (required): Population by age group (same format as deaths)
- `ages` (required): Age group start values, comma-separated (e.g., `0,15,65,85`)
- `period` (optional): Data period type, default `yearly`
  - `yearly` = Annual data (no STL decomposition)
  - `quarterly` = Quarterly data
  - `monthly` = Monthly data
  - `weekly` = Weekly data
- `sex` (optional): Sex for nax estimation, default `t`
  - `m` or `male` = Male
  - `f` or `female` = Female
  - `t` or `total` = Both sexes combined

**Example Request (Single Period):**
```bash
curl "http://localhost:5000/lt?deaths=500,1000,5000,10000&population=1000000,3000000,800000,200000&ages=0,15,65,85"
```

**Response (Single Period):**
```json
{
  "e0": 78.5,
  "e65": 18.2,
  "trend": null,
  "seasonal": null,
  "adjusted": null
}
```

**Example Request (Multiple Monthly Periods):**
```bash
curl "http://localhost:5000/lt?deaths=500,1000,5000,10000;510,1020,5100,10200;490,980,4900,9800&population=1000000,3000000,800000,200000;1000000,3000000,800000,200000;1000000,3000000,800000,200000&ages=0,15,65,85&period=monthly"
```

**Response Fields:**
- `e0`: Life expectancy at birth (single value or array for multiple periods). Rounded to 2 decimal places.
- `e65`: Life expectancy at age 65 (single value or array). Rounded to 2 decimal places.
- `trend`: STL trend component (only for sub-yearly data with 2+ years). Rounded to 2 decimal places.
- `seasonal`: STL seasonal component. Rounded to 3 decimal places.
- `adjusted`: Seasonally adjusted e0 values (e0 minus seasonal component). Rounded to 2 decimal places.

**STL Decomposition Requirements:**
- Monthly data: 24+ periods (2 years)
- Weekly data: 104+ periods (2 years)
- Quarterly data: 8+ periods (2 years)
- Yearly data: No STL (returns null for trend/seasonal/adjusted)

**Methodology:**
Pure R implementation of Chiang's method for abridged life tables. Key features:
- **nax estimation**: Coale-Demeny coefficients for infant mortality, UN method for other ages
- **85+ closure**: Keyfitz method (Lx = lx / Mx) for open-ended intervals
- **No external dependencies**: Works in any R environment without compilation

Age groups are automatically detected from the `ages` parameter - works with any standard format (5-year, 10-year, etc.).

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

## Security & Monitoring

### Sentry Error Tracking

The service supports Sentry integration for production error monitoring and performance tracking.

**Setup:**

1. Create a Sentry project at [sentry.io](https://sentry.io/) or use a self-hosted instance (e.g., Bugsink)
2. Get your DSN from the project settings
3. Configure environment variables:

   **For local development:**
   ```bash
   export SENTRY_DSN="https://your-key@your-org.ingest.sentry.io/your-project-id"
   export SENTRY_ENVIRONMENT="development"
   export SENTRY_TRACES_SAMPLE_RATE="0.1"
   ```

   **For production (Dokku):**
   Environment variables are automatically configured via `deployments/config.json`:
   ```json
   "env_vars": {
     "SENTRY_DSN": "https://key@sentry.mortality.watch/3",
     "SENTRY_ENVIRONMENT": "production",
     "SENTRY_TRACES_SAMPLE_RATE": "0.1"
   }
   ```

**Features:**
- Automatic exception capture with stack traces
- Request context (path, query params, IP)
- Environment and server metadata
- Custom tags for filtering (endpoint, method)

**Logs:**
```
[2025-10-26 12:00:00] INFO: Sentry initialized - environment=production, traces_sample_rate=0.1
```

If `SENTRY_DSN` is not configured, error tracking is disabled and the service runs normally.

### CORS (Cross-Origin Resource Sharing)

The service implements strict CORS policies to only allow requests from whitelisted origins.

**Default allowed origins:**
- `https://www.mortality.watch`
- `https://mortality.watch`
- `http://localhost:3000` (local dev)
- `http://localhost:3001` (local dev)
- `http://127.0.0.1:3000` (local dev)

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
│   ├── utils.r           # Statistical utility functions
│   └── sentry.r          # Error tracking integration
├── tests/
│   ├── test_utils.r      # Tests for utility functions
│   ├── test_validation.r # Tests for validation/caching
│   ├── test_integration.r # API integration tests
│   └── run_tests.r       # Test runner
├── dependencies_r.txt    # R package dependencies
├── dependencies.txt      # System dependencies
├── install_r_deps.sh     # Dependency installer
├── Dockerfile            # Container configuration
├── .env.example          # Environment variables template
├── CONTRIBUTING.md       # Contribution guidelines
└── README.md            # This file
```

## Testing

### Unit Tests

Run unit tests for utilities and validation:

```bash
cd tests
Rscript test_utils.r
Rscript test_validation.r
```

### Integration Tests

Integration tests require a running server:

```bash
# Terminal 1: Start the server
Rscript src/serve.r

# Terminal 2: Run integration tests
cd tests
Rscript test_integration.r
```

Tests cover:
- Statistical utility functions
- Input validation
- Caching logic
- Rate limiting
- Error handling
- Full API workflows
- CORS functionality

## Deployment

### Dokku

The service is configured for Dokku deployment with nginx plugins:

```bash
# Deploy
git push dokku master

# Enable CORS
dokku nginx-cors:enable stats-mortality-watch

# Enable caching
dokku nginx-cache:enable stats-mortality-watch
```

**Port Configuration:**
- The service runs on port 5000 internally (configurable via `PORT` env var)
- Dokku automatically proxies external port 80 to internal port 5000
- No explicit port mapping needed in `deployments/config.json`

**Environment Variables:**
Environment variables (including Sentry configuration) are automatically configured via the central deployment system at `~/dev/co/deployments/config.json`.

Pre-deployment script: `pre-deploy.sh`

### Docker Compose

```yaml
version: '3.8'
services:
  stats:
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

### v1.1.0 (TBD)

**New Features:**
- ✨ Sentry integration for error tracking and monitoring
- ✨ Integration tests for full API workflow testing
- ✨ CONTRIBUTING.md with contribution guidelines

**Improvements:**
- 🔒 Docker now runs as non-root user for security
- ✅ CI now runs all tests (including test_validation.r)
- ✅ Linting now enforced in CI (no longer optional)
- 📝 Added .env.example template for environment variables
- 🐛 Fixed pre-deploy.sh variable inconsistency
- 🧹 Removed unused future.apply dependency

**Breaking Changes:**
- None (API remains backwards compatible)

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
