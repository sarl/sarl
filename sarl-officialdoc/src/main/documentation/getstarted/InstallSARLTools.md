# Install SARL Tools

[:Outline:]

There are two straightforward methods to get SARL up and running. You can either use a pre-configured Eclipse IDE distribution, which comes with all the necessary plug-ins already installed, or you can install the SARL SDK into your existing Eclipse setup using the Eclipse update mechanism. This guide focuses on the first method for installing SARL.

## Step 1: Install Java Development Kit (JDK) version [:sarl-run.min.jdk.version!]

The SARL product requires Java [:sarl-run.min.jdk.version!] or higher to run. Download the Java Development Kit (JDK) from a standard provider, such as [Oracle](https://www.oracle.com/java/technologies/downloads/) or [OpenJDK](https://openjdk.org/).

## Step 2: Download the SARL product

Follow the instructions on the [download page]([:sarl.url!]/download/index.html) to download the SARL tools. Select the product that matches your operating system.

> **Note for MacOS Users:** On some versions of MacOS, downloading the SARL product through a web browser may corrupt the file, making it impossible to run. In such cases, we recommend using a command-line tool like [wget](https://www.gnu.org/software/wget/) to download the SARL product.

## Step 3: Uncompress the downloaded SARL product

After downloading, you need to uncompress the SARL product to access the executable files. This step is mandatory.

## Step 4: Configure the SARL product for using the correct JDK

Since SARL requires Java to run and multiple Java environments might be installed on your system, it's best to specify the correct JDK for the SARL tools.

**Assumptions:**
1. Your JDK is installed in a folder with the path `[:jdkpath](/path/to/jdk)`. This folder should contain a `bin` subfolder with the `java` and `javac` tools.
2. The SARL product is uncompressed in the folder `[:sarlpath](/path/to/sarl)`.

**Steps to Configure the JDK:**

1. Open the file `[:sarlpath!]/sarlide.ini` in a text editor, such as Notepad.
2. Locate the line containing `-vm`. If you can't find this line, proceed to step 6. Otherwise, continue to step 3.
3. The line following `-vm` should contain a path to the JDK folder.
4. Replace the existing path with `[:jdkpath!]/bin`. Ensure the path ends with `bin`.
5. Skip to step 8.
6. At the end of the file, add the line `-vm` without any additional text on this line.
7. Directly after the `-vm` line, add a new line with the path to your JDK: `[:jdkpath!]/bin`. Ensure the path ends with `bin`.
8. Save the changes to the `sarlide.ini` file.

## Step 5: Run the SARL product

You can now launch the SARL development environment by executing the binary file located in the [:sarlpath:] folder. The binary file is named `sarlide`. Specific launching scripts may be provided for your operating system, such as Linux Ubuntu or Arch Linux.

## What's next?

In the next section, we will learn how to create a SARL project.

[Next>](./CreateFirstProject.md)

[:Include:](../includes/legal.inc)
