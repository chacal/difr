USE_CSC_NEXUS="-Dsbt.override.build.repos=true -Dsbt.repository.config=repositories"
SBT_OPTS="-Xms128M -Xmx2G -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M"
java $SBT_OPTS $USE_CSC_NEXUS -jar `dirname $0`/sbt-launch.jar "$@"
