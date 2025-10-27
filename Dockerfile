FROM eddelbuettel/r2u:22.04

# Metadata
LABEL maintainer="MortalityWatch"
LABEL description="R Statistical Forecasting Microservice"
LABEL version="1.0.0"

WORKDIR /opt/stats

# Install system dependencies
RUN echo "Updating deps... $CACHEBUST"
RUN apt-get update && apt-get install -y curl && rm -rf /var/lib/apt/lists/*

# Copy and install dependencies (cached layer)
COPY dependencies.txt .
RUN if [ -s dependencies.txt ]; then apt-get update && apt-get install -y $(cat dependencies.txt) && rm -rf /var/lib/apt/lists/*; fi

# Install R dependencies (cached layer)
COPY dependencies_r.txt .
COPY install_r_deps.sh .
RUN /opt/stats/install_r_deps.sh

# Copy application code
COPY src/ .

# Expose port
EXPOSE 5000

# Health check
HEALTHCHECK --interval=30s --timeout=5s --retries=3 \
  CMD curl -f http://localhost:5000/health || exit 1

# Run as non-root user for security
# Use UID 10000 to avoid conflicts with base image
RUN useradd -m -u 10000 stats && chown -R stats:stats /opt/stats
USER stats

# Start server
CMD ["Rscript", "serve.r"]
