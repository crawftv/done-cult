# Use Debian as the base image
FROM 637423200832.dkr.ecr.us-east-1.amazonaws.com/crawfordc:crawford-base AS done_cult

RUN apt-get update && apt-get install -y \
    nginx \
    && rm -rf /var/lib/apt/lists/*

FROM done_cult AS runtime_base

# Copy Nginx configuration
COPY nginx.conf                 /etc/nginx/nginx.conf

# Copy elm files
COPY public/                    /app/public/
COPY index.html                 /app/index.html
RUN chmod -R 755 /app/public

# Copy supervisord configuration
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf

# Expose port 80 for Nginx
EXPOSE 80

CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]