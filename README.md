# genomics-invoicing
This is a project intended to automate the generation of genomic invoices to customers.
## For future developers
### Windows users
#### 1. Setting up Windows Subsystem for Linux (WSL)
There are plenty of tutorials online, won't go into details here. Remember your WSL **username** and **password**, you will need it later.
#### 2. Install R
The author of this entry uses **Debian distribution** of WSL, the installation process may be different depending on your WSL distro. See more [here](https://cloud.r-project.org/index.html)

To ensure we are using the same R version as the server, we would need to install an older version of R: 4.4.1. Run this command to start a text file to tell your system where to find this version:
```bash
sudo nano /etc/apt/sources.list.d/cran-r.list
```
In the window that pops up, put in the following command. Note: this is for Debian Bookworm ONLY!
```bash
deb http://cloud.r-project.org/bin/linux/debian bookworm-cran40/
```
Now type **crtl-O** to write the line and **crtl-X** to exit.

Next, run this to get identification for the download:
```bash
gpg --keyserver keyserver.ubuntu.com \
    --recv-key '95C0FAF38DB3CCAD0C080A7BDC78B2DDEABC47B7'
```
And this command to add the identificatino to your system's recognised id:
```bash
gpg --armor --export '95C0FAF38DB3CCAD0C080A7BDC78B2DDEABC47B7' | \
    sudo tee /etc/apt/trusted.gpg.d/cran_debian_key.asc
```
Now you are set with the repository. Check the validity of the repository with:
```bash
sudo apt-get update
apt-cache policy r-base
```
If you see "4.4.1-1~bookwormcran.0" in "candidates", you are all set.
Now install R 4.4.1 with:
```bash
sudo apt-get install r-base=4.4.1-1~bookwormcran.0
```
Finally check installation with:
```bash
R --version
```
#### 3. Install R studio server
Now install the latest R studio desktop. Go to the [official R-studio site](https://posit.co/download/rstudio-server/). **IMPORTANT: Select your distro and version correctly**. Follow step 3. Code is not shown here because the download link may change upon updates. 
#### 4. Launch dev space
Run this command in your bash shell
```bash
sudo rstudio-server start
```
if you see some text starting with "TTY detected.", you are good to go. 
Now open your browser and type in:
```bash
http://localhost:8787
```
you should see a window asking for a user name and password. **Use your WSL username and password**. Then you can see your development environment in your browser successfully deployed.
### MAC users
Mac Terminal does not support .rpm packages. **Make sure the R application is downloaded on you macboook** (check [here](https://cran.r-project.org/bin/macosx/)). <br />
Follow the instructions below to access the R studio server on Macbook.
#### 1. Download Docker Application
The instructions to download Docker can be found on their website https://docs.docker.com/desktop/setup/install/mac-install/#install-and-run-docker-desktop-on-mac
#### 2. Run command on Docker Terminal
Run the following command to enable RStudio server to run on your browser:
```bash
docker run -e PASSWORD=mypassword -p 8787:8787 rocker/rstudio
```
Note: Set ```mypassword``` to any password you want
#### 3. Run RStudio Server on your browser
Use the address ```http://localhost:8787```
#### 4. Log in credentials
Username: rstudio 
Password: the ```mypassword``` you set in step 2
