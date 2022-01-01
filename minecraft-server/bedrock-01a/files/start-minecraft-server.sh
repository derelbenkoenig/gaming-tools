#!/bin/bash

mkdir -p /var/lib/minecraft/data/mods

# -dit means detach, interactive, pseudo tty. This allows you to attach and detach from stdin
#
# 3G memory should leave some room within the 4G that the VM has?
#
# OVERRIDE_SERVER_PROPERTIES is needed for the ALLOW_FLIGHT et al to be added to server.properties if
# 	server.properties already exists
#
# LEVEL_TYPE=BIOMESOP needed for biomes o' plenty to actually generate special biomes
docker run \
	-dit \
	--restart="on-failure" \
	-v /var/lib/minecraft/data:/data:Z \
	-p 19132:19132 \
	-u 5000:5000 \
	-e VERSION=LATEST \
	-e SEED="-479112237913370" \
	-e OPS="DerElbenkoenig" \
	-e OVERRIDE_OPS=true \
	-e EULA=TRUE \
	-e UID=5000 \
	-e GID=5000 \
	-e MEMORY=3G \
	-e OVERRIDE_SERVER_PROPERTIES=TRUE \
	--name minecraftserver \
	docker.io/itzg/minecraft-bedrock-server:latest
