# Visualizing Data

## R & Apache Zeppelin

```
sudo apt-get update
sudo apt-get install git
sudo apt-get install openjdk-7-jdk
sudo apt-get install npm
sudo apt-get install libfontconfig
```

### Install Maven

```
# install maven
wget http://www.eu.apache.org/dist/maven/maven-3/3.3.3/binaries/apache-maven-3.3.3-bin.tar.gz
sudo tar -zxf apache-maven-3.3.3-bin.tar.gz -C /usr/local/
sudo ln -s /usr/local/apache-maven-3.3.3/bin/mvn /usr/local/bin/mvn
```

### Install R
```
sudo apt-get install r-base-core
```
R> install.packages("evaluate", dependencies = TRUE)
R> install.packages("base64enc", dependencies = TRUE)
```
Install Packages
```
sudo apt-get install libcurl4-openssl-dev
sudo apt-get install libxml2-dev
```
```
R> install.packages("devtools", dependencies = TRUE)
R> library('devtools')
R> install_github("IRkernel/repr")
```
### Build
```
cd Desktop/Apache/incubator-zeppelin-rinterpreter
mvn clean package -DskipTests
```

### Start
```
$ ./bin/zeppelin-daemon.sh start
```
