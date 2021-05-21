#!/bin/bash

workdir=$(dirname ${BASH_SOURCE[0]})
echo $workdir

scp ${workdir}/files/start-minecraft-server.sh minecraft-forge-01a:/root/scripts/start-minecraft-server.sh

# scp ${workdir}/files/mods.zip minecraft-forge-01a:/root/
