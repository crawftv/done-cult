aws ecr get-login-password --region us-east-1 | docker login --username AWS --password-stdin $AWS_REGISTRY_LOCATION; \
docker service rm done; \
docker service create \
  --name done \
  --mount type=bind,source=$HOME/.cloudflared/,target=/app/cloudflared,readonly \
  $AWS_REGISTRY_LOCATION/crawfordc:done

