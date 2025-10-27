# Contributing to R Stats Microservice

Thank you for your interest in contributing to the R Stats Microservice! This document provides guidelines and instructions for contributing to the project.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Workflow](#development-workflow)
- [Coding Standards](#coding-standards)
- [Testing](#testing)
- [Pull Request Process](#pull-request-process)
- [Reporting Issues](#reporting-issues)

## Code of Conduct

This project adheres to a code of conduct that all contributors are expected to follow:

- **Be respectful**: Treat all contributors with respect and courtesy
- **Be collaborative**: Work together constructively to improve the project
- **Be inclusive**: Welcome newcomers and help them get started
- **Be professional**: Keep discussions focused on technical merit

## Getting Started

### Prerequisites

- **R 4.x** or higher
- **Docker** (optional, for containerized development)
- **Git** for version control
- Basic familiarity with statistical forecasting and time series analysis

### Setting Up Your Development Environment

1. **Fork the repository** on GitHub

2. **Clone your fork**:
   ```bash
   git clone https://github.com/YOUR_USERNAME/stats.mortality.watch.git
   cd stats.mortality.watch
   ```

3. **Add upstream remote**:
   ```bash
   git remote add upstream https://github.com/MortalityWatch/stats.mortality.watch.git
   ```

4. **Install dependencies**:
   ```bash
   ./install_r_deps.sh
   ```

5. **Copy environment variables**:
   ```bash
   cp .env.example .env
   # Edit .env with your configuration
   ```

6. **Run the server locally**:
   ```bash
   Rscript src/serve.r
   ```

7. **Verify it's working**:
   ```bash
   curl http://localhost:5000/health
   ```

## Development Workflow

### Branching Strategy

- `master` - Production-ready code
- `feature/*` - New features
- `fix/*` - Bug fixes
- `docs/*` - Documentation updates
- `test/*` - Test improvements

### Working on a Feature

1. **Sync with upstream**:
   ```bash
   git checkout master
   git pull upstream master
   ```

2. **Create a feature branch**:
   ```bash
   git checkout -b feature/your-feature-name
   ```

3. **Make your changes** following the coding standards below

4. **Run tests** to ensure everything works:
   ```bash
   cd tests
   Rscript test_utils.r
   Rscript test_validation.r

   # Start server in another terminal, then:
   Rscript test_integration.r
   ```

5. **Commit your changes**:
   ```bash
   git add .
   git commit -m "feat: add your feature description"
   ```

6. **Push to your fork**:
   ```bash
   git push origin feature/your-feature-name
   ```

7. **Open a Pull Request** on GitHub

## Coding Standards

### R Code Style

We follow the [Tidyverse Style Guide](https://style.tidyverse.org/) with some modifications:

#### Naming Conventions

- **Functions**: Use `snake_case` or `camelCase` (be consistent within a file)
- **Variables**: Use `snake_case`
- **Constants**: Use `UPPER_SNAKE_CASE`

#### Function Documentation

Use roxygen2-style comments for all exported functions:

```r
#' Brief description of the function
#'
#' Longer description with more details about what the function does
#'
#' @param param1 Description of first parameter
#' @param param2 Description of second parameter
#' @return Description of return value
#' @examples
#' my_function(10, 20)
my_function <- function(param1, param2) {
  # Implementation
}
```

#### Line Length

- Maximum line length: **120 characters**
- Break long lines at logical points

#### Indentation

- Use **2 spaces** for indentation (no tabs)

#### Error Handling

- Always use `tryCatch()` for operations that might fail
- Provide descriptive error messages
- Log errors appropriately

#### Example

```r
#' Validate request parameters
#'
#' @param query Query parameters from request
#' @return List with valid=TRUE/FALSE and error message if invalid
validate_request <- function(query) {
  if (is.null(query$y)) {
    return(list(valid = FALSE, message = "Missing required parameter 'y'"))
  }

  y <- tryCatch(
    as.double(strsplit(as.character(query$y), ",")[[1]]),
    error = function(e) NULL
  )

  if (is.null(y)) {
    return(list(valid = FALSE, message = "Parameter 'y' must be numeric"))
  }

  return(list(valid = TRUE))
}
```

### Project Structure

```
.
├── src/
│   ├── serve.r           # Main server (routing, middleware)
│   ├── handlers.r        # Request handlers (business logic)
│   ├── utils.r           # Utility functions (pure functions)
│   └── sentry.r          # Error tracking integration
├── tests/
│   ├── test_utils.r      # Unit tests for utilities
│   ├── test_validation.r # Tests for validation logic
│   └── test_integration.r # API integration tests
└── dependencies_r.txt    # R package dependencies
```

### Adding New Features

When adding a new feature:

1. **Add utility functions** to `src/utils.r` if needed
2. **Add request handler** to `src/handlers.r`
3. **Update validation** in `src/serve.r` if new parameters are needed
4. **Add routing** in `src/serve.r` main request handler
5. **Write tests** in appropriate test files
6. **Update README.md** with new API documentation
7. **Update CHANGELOG** in README.md

## Testing

### Test Types

#### Unit Tests (`test_utils.r`, `test_validation.r`)

Test individual functions in isolation:

```r
test_that("Function handles valid input", {
  result <- my_function(valid_input)
  expect_equal(result, expected_output)
})

test_that("Function handles invalid input", {
  expect_error(my_function(invalid_input))
})
```

#### Integration Tests (`test_integration.r`)

Test the full API workflow with a running server:

```r
test_that("Endpoint returns correct response", {
  response <- GET(paste0(BASE_URL, "/endpoint"), query = list(...))
  expect_equal(status_code(response), 200)

  body <- content(response, as = "parsed")
  expect_true("field" %in% names(body))
})
```

### Running Tests

```bash
# Unit tests
cd tests
Rscript test_utils.r
Rscript test_validation.r

# Integration tests (requires running server)
# Terminal 1:
Rscript src/serve.r

# Terminal 2:
cd tests
Rscript test_integration.r
```

### Test Coverage

- Aim for **80%+ code coverage**
- All new features must include tests
- All bug fixes must include regression tests

## Pull Request Process

### Before Submitting

1. ✅ **Tests pass**: All existing tests continue to pass
2. ✅ **New tests added**: New features have corresponding tests
3. ✅ **Linting passes**: Code follows style guidelines
4. ✅ **Documentation updated**: README and comments are up to date
5. ✅ **No merge conflicts**: Rebase on latest master if needed

### PR Description Template

```markdown
## Description
Brief description of what this PR does

## Type of Change
- [ ] Bug fix (non-breaking change fixing an issue)
- [ ] New feature (non-breaking change adding functionality)
- [ ] Breaking change (fix or feature causing existing functionality to change)
- [ ] Documentation update

## Testing
- [ ] Unit tests added/updated
- [ ] Integration tests added/updated
- [ ] Manual testing performed

## Checklist
- [ ] Code follows project style guidelines
- [ ] Self-review performed
- [ ] Comments added for complex code
- [ ] Documentation updated
- [ ] No new warnings generated
- [ ] Tests added and passing
```

### Review Process

1. **Automated checks**: CI must pass (tests, linting, Docker build)
2. **Code review**: At least one maintainer approval required
3. **Testing**: Reviewers may test changes locally
4. **Feedback**: Address all review comments
5. **Merge**: Maintainer will merge when approved

### Commit Message Format

We use [Conventional Commits](https://www.conventionalcommits.org/):

```
<type>: <description>

[optional body]

[optional footer]
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `test`: Adding or updating tests
- `refactor`: Code refactoring
- `perf`: Performance improvements
- `chore`: Maintenance tasks

**Examples:**
```
feat: add exponential smoothing forecast method

fix: resolve cache key collision for different query orders

docs: update API documentation for /cum endpoint

test: add integration tests for rate limiting
```

## Reporting Issues

### Bug Reports

When reporting bugs, include:

1. **Description**: Clear description of the bug
2. **Steps to reproduce**: Exact steps to trigger the bug
3. **Expected behavior**: What should happen
4. **Actual behavior**: What actually happens
5. **Environment**:
   - R version
   - Operating system
   - Docker version (if applicable)
6. **Logs**: Relevant error messages or logs
7. **Screenshots**: If applicable

### Feature Requests

When requesting features, include:

1. **Use case**: Why is this feature needed?
2. **Proposed solution**: How should it work?
3. **Alternatives**: Other approaches considered
4. **Additional context**: Any other relevant information

## Questions?

- **Issues**: Open a GitHub issue for questions
- **Discussions**: Use GitHub Discussions for broader topics
- **Security**: For security issues, email [security contact] directly

## License

By contributing, you agree that your contributions will be licensed under the AGPL-3.0 License.

---

Thank you for contributing to R Stats Microservice!
