This project defines jribble AST, syntax and provides jribble parsers and printers.

In order to build, you need to have simple build tool (sbt) installed.
Installation is easy as executing following commands (on Linux):

    wget http://simple-build-tool.googlecode.com/files/sbt-launch-0.7.4.jar -O ~/bin/sbt-launch-0.7.4.jar
    echo 'java -Xmx512M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m -jar `dirname $0`/sbt-launch-0.7.4.jar "$@"' > ~/bin/sbt
    chmod u+x ~/bin/sbt
    echo 'export PATH=$PATH:~/bin' >> ~/.bashrc
    source ~/.bashrc

or on Mac:

    wget http://simple-build-tool.googlecode.com/files/sbt-launch-0.7.4.jar -O ~/bin/sbt-launch-0.7.4.jar
    echo 'java -Xmx512M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m -jar `dirname $0`/sbt-launch-0.7.4.jar "$@"' > ~/bin/sbt
    chmod u+x ~/bin/sbt
    echo 'export PATH=$PATH:~/bin' >> ~/.bash_profile
    source ~/.bash_profile

then type 'sbt' and you should see sbt's console.  Useful commands are 'compile' and 'test'.

For more information consult sbt's homepage: http://code.google.com/p/simple-build-tool/
