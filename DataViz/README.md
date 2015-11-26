# Visualizing Data

## Configure Ubuntu 

### Resize disk
- Clone the .vmdk image to a .vdi.
- Resize the new .vdi image (30720 == 30 GB).
- Optional; switch back to a .vmdk.
```
vboxmanage clonehd "virtualdisk.vmdk" "new-virtualdisk.vdi" --format vdi
vboxmanage modifyhd "new-virtualdisk.vdi" --resize 30720
VBoxManage clonehd "cloned.vdi" "resized.vmdk" --format vmdk
```

### Create a swap partition using [GNOME Partition Editor - GParted](http://gparted.org/livecd.php) and configure it
- Get the UUID for the partition
- Edit fstab and add/modify UUID=3aec9d69-737e-4840-94ca-45d01f2aed05 none   swap    sw      0       0
- Check if enabled after reboot
```
sudo blkid /dev/sda2
sudo nano /etc/fstab
cat /proc/meminfo
```


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
R
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
