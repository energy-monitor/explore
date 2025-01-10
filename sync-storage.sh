#!/usr/bin/env bash

BASE_FOLDER=`dirname -- "$0"`;
cd $BASE_FOLDER

rclone bisync google-drive:/energy.wifo.ac.at/energy-monitor-data data/storage

