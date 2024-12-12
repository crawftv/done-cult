# Use Debian as the base image
FROM 637423200832.dkr.ecr.us-east-1.amazonaws.com/crawfordc:crawford-base AS done_cult


COPY public/                    /app/
COPY public/index.html          /app/index.html
COPY public/elm.js              /app/elm.js
COPY server/server.py                  /app/server.py
RUN pip install fastapi authlib uvicorn
RUN chmod -R 755 /app

# Copy supervisord configuration
COPY supervisord.conf /etc/supervisor/conf.d/supervisord.conf

CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]