#!/bin/bash

mkdir -p /var/lib/minecraft/data/mods

# -dit means detach, interactive, pseudo tty. This allows you to attach and detach from stdin
# 3G memory should leave some room within the 4G that the VM has?
docker run \
	-dit \
	-v /var/lib/minecraft/data:/data:Z \
	-p 25565:25565 \
	-u 5000:5000 \
	-e VERSION=1.12.2 \
	-e SEED="-479112237913370" \
	-e OPS="DerElbenkoenig" \
	-e OVERRIDE_OPS=true \
	-e TYPE=FORGE \
	-e EULA=TRUE \
	-e UID=5000 \
	-e GID=5000 \
	-e MEMORY=3G \
	-e ALLOW_FLIGHT=TRUE \
	-e OVERRIDE_SERVER_PROPERTIES=TRUE \
	--name minecraftserver \
	docker.io/itzg/minecraft-server:java8
