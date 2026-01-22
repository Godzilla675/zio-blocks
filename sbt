#!/bin/bash
SBT_VERSION="1.12.0"
SBT_JAR="sbt-launch-${SBT_VERSION}.jar"
SBT_URL="https://repo1.maven.org/maven2/org/scala-sbt/sbt-launch/${SBT_VERSION}/sbt-launch-${SBT_VERSION}.jar"

if [ ! -f "$SBT_JAR" ]; then
  echo "Downloading sbt launch jar..."
  if command -v curl >/dev/null 2>&1; then
    curl -L -o "$SBT_JAR" "$SBT_URL"
  elif command -v wget >/dev/null 2>&1; then
    wget -O "$SBT_JAR" "$SBT_URL"
  else
    echo "Error: Neither curl nor wget found. Cannot download sbt."
    exit 1
  fi
fi

java -Xmx2G -jar "$SBT_JAR" "$@"
