#!/bin/bash

mkdir -p /var/lib/minecraft/data/
chown 5000:5000 /var/lib/minecraft -R

# -dit means detach, interactive, pseudo tty. This allows you to attach and detach from stdin
#
# 3G memory should leave some room within the 4G that the VM has?
#
docker run \
	-dit \
	--restart="on-failure" \
	-v /var/lib/minecraft/data:/data:Z \
	-p 19132:19132/udp \
	-e VERSION=LATEST \
	-e SEED="-479112237913370" \
	-e OPS="DerElbenkoenig" \
	-e OVERRIDE_OPS=true \
	-e EULA=TRUE \
	-e MEMORY=3G \
	--name minecraftserver \
	docker.io/itzg/minecraft-bedrock-server:latest
