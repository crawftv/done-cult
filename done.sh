 docker service create \
  --name done \
  --mount type=bind,source=$HOME/.cloudflared/,target=/app/cloudflared,readonly \
  $AWS_REGISTRY_LOCATION/crawfordc:done