# 2 Software Installation Guide

## 2.1 Operation system/software/hardware requirement

MCPSCU软件从版本mcpscu_2017_09_30至mcpscu_2018_03_20，支持Windows系统下的运行，从mcpscu_2018_05_14至最新版本，逐渐迁移至Linux以及类Linux环境下的开发。
对于最新的mcpscu_2021_07_02版本，目前支持Linux以及基于CYGWIN环境的Windows两种操作环境下的编译、运行。

### 2.1.1 Linux

要在Linux操作系统下进行编译、运行MCPSCU，所需软件和硬件如下表所示。说明：安装PGI CE Linux时，其会询问是否安装CUDA。
这儿建议将CUDA与PGI分开安装。查阅PGI官网确定适配的CUDA版本，先独立安装CUDA，再独立安装PGI，安装PGI时选择不安装CUDA。

由于MCPSCU通过静态调用方式来使用TinyC，因此需要TinyC编译生成静态库。因此，需要在编译生成TinyC之前，使用：
```bash
./configure --enable-static
```
然后再执行make命令。这样才会生成静态库libtcc.lib。


Table 2.1 Linux编译、运行MCPSCU环境需求。
Hardwares:

| Name       | Minimum Configuration | Recommended Configuration |
| ---------- | --------------------- | ------------------------- |
| CPU        | Intel(R) Core i3      | Intel(R) Xeon series      |
| GPU        | NVIDIA Tesla C2050    | NVIDIA Tesla V100         |
| Memory     | 500 MB                | 50 GB+                    |
| Disk Space | 100 MB                | 1 TB+                     |

Softwares:

| Name          | Minimum Version   | Recommended Version |
| ------------- | ----------------- | ------------------- |
| MCPSCU        | mcpscu_2018_03_21 | mcpscu_2021_07_02   |
| GNU Make      | 3.81              | 3.82                |
| NVIDIA Driver | 375.26            | 440.64              |
| CUDA          | 8.0               | 10.2                |
| PGI CE Linux  | 17.4              | 19.4                |
| g++           | 4.47              | 4.85                |
| Tiny C        | 0.9.27            | 0.9.27              |
| GNU tar       | 1.23              | 1.26                |


### 2.1.2 Windows

| Name    | Minimum Version | Recommended Version |
| ------- | --------------- | ------------------- |
| Windows | Windows 7       | Windows Server 2012 |

要在Windows操作系统下进行编译、运行MCPSCU，所需软件和硬件如下表所示。说明：安装PGI
CE
Win时，其会询问是否安装CUDA。这儿建议将CUDA与PGI分开安装。查阅PGI官网以确定适配的CUDA版本，先独立安装CUDA，再独立安装PGI，安装PGI时选择不安装CUDA。

特别需要注意的是，为了使MCPSCU正常使用，安装PGI CE
Win时需要用勾选并安装Cygwin环境，完成后，双击快捷图标：可以进入Cygwin环境。

此外，由于MCPSCU中需要使用TinyC，因此需要安装TinyC。由于安装PGI CE
Win时已经要求安装Microsoft Visual Studio 2017(C++)，因此，打开"VS 2017
的开发者命令提示符"(位于开始菜单-\>Visual studio
2017)，使用命令进入解压后的TinyC路径PathTinyC
(例如：/xxx/tcc-0.9.27)，执行如下操作：
```powershell
xxx\tcc-0.9.27>cd win32
xxx\tcc-0.9.27\win32> build-tcc.bat -c cl
```
编译完成后，/xxx/tcc-0.9.27/win32目录下应该包含如下文件：
```
├── build-tcc.bat
├── doc
│   └── tcc-win32.txt
├── examples
│   ├── dll.c
│   ├── fib.c
│   ├── hello_dll.c
│   ├── hello_win.c
│   └── libtcc_test.c
├── i386-win32-tcc.exe
├── i386-win32-tcc.pdb
├── include
│   ├── _mingw.h
│   ├── assert.h
│   ├── conio.h
│   ├── ctype.h
│   ├── dir.h
│   ├── direct.h
│   ├── dirent.h
│   ├── dos.h
│   ├── errno.h
│   ├── excpt.h
│   ├── fcntl.h
│   ├── fenv.h
│   ├── float.h
│   ├── inttypes.h
│   ├── io.h
│   ├── limits.h
│   ├── locale.h
│   ├── malloc.h
│   ├── math.h
│   ├── mem.h
│   ├── memory.h
│   ├── process.h
│   ├── sec_api
│   │   ├── conio_s.h
│   │   ├── crtdbg_s.h
│   │   ├── io_s.h
│   │   ├── mbstring_s.h
│   │   ├── search_s.h
│   │   ├── stdio_s.h
│   │   ├── stdlib_s.h
│   │   ├── stralign_s.h
│   │   ├── string_s.h
│   │   ├── sys
│   │   │   └── timeb_s.h
│   │   ├── tchar_s.h
│   │   ├── time_s.h
│   │   └── wchar_s.h
│   ├── setjmp.h
│   ├── share.h
│   ├── signal.h
│   ├── stdarg.h
│   ├── stdbool.h
│   ├── stddef.h
│   ├── stdint.h
│   ├── stdio.h
│   ├── stdlib.h
│   ├── string.h
│   ├── sys
│   │   ├── fcntl.h
│   │   ├── file.h
│   │   ├── locking.h
│   │   ├── stat.h
│   │   ├── time.h
│   │   ├── timeb.h
│   │   ├── types.h
│   │   ├── unistd.h
│   │   └── utime.h
│   ├── tcc
│   │   └── tcc_libm.h
│   ├── tcclib.h
│   ├── tchar.h
│   ├── time.h
│   ├── vadefs.h
│   ├── values.h
│   ├── varargs.h
│   ├── wchar.h
│   ├── wctype.h
│   └── winapi
│       ├── basetsd.h
│       ├── basetyps.h
│       ├── guiddef.h
│       ├── poppack.h
│       ├── pshpack1.h
│       ├── pshpack2.h
│       ├── pshpack4.h
│       ├── pshpack8.h
│       ├── winbase.h
│       ├── wincon.h
│       ├── windef.h
│       ├── windows.h
│       ├── winerror.h
│       ├── wingdi.h
│       ├── winnt.h
│       ├── winreg.h
│       ├── winuser.h
│       └── winver.h
├── lib
│   ├── chkstk.S
│   ├── crt1.c
│   ├── crt1w.c
│   ├── dllcrt1.c
│   ├── dllmain.c
│   ├── gdi32.def
│   ├── kernel32.def
│   ├── libtcc1-32.a
│   ├── libtcc1-64.a
│   ├── msvcrt.def
│   ├── user32.def
│   ├── wincrt1.c
│   └── wincrt1w.c
├── libtcc
│   ├── libtcc.def
│   └── libtcc.h
├── libtcc.dll
├── libtcc.exp
├── libtcc.lib
├── libtcc.obj
├── libtcc.pdb
├── tcc-win32.txt
├── tcc.exe
├── tcc.obj
├── tcc.pdb
└── vc140.pdb
```

Table 2.2 Windows编译、运行MCPSCU环境需求。
| Name       | Minimum Configuration | Recommended Configuration |
| ---------- | --------------------- | ------------------------- |
| CPU        | Intel(R) Core i3      | Intel(R) Xeon series      |
| GPU        | NVIDIA Tesla C2050    | NVIDIA Tesla V100         |
| Memory     | 500 MB                | 50 GB+                    |
| Disk Space | 100 MB                | 1 TB+                     |

Softwares:

| Name          | Minimum Version   | Recommended Version |
| ------------- | ----------------- | ------------------- |
| MCPSCU        | mcpscu_2018_03_21 | mcpscu_2021_07_02   |
| GNU Make      | 3.81              | 3.82                |
| NVIDIA Driver | 375.26            | 440.64              |
| CUDA          | 8.0               | 10.2                |
| PGI CE （(include CYGWIN)）  | 17.4              | 19.4                |
| g++           | 4.47              | 4.85                |
| Tiny C        | 0.9.27            | 0.9.27              |
| GNU tar       | 1.23              | 1.26                |
|CYGWIN (included in PGI CE Win)	|1.7.27	|3.2.0          |
|Microsoft Visual Studio(required by PGI CE Win)| 	2015	|2017|

## 2.2 Install Steps

### 2.2.1 In Linux

#### 2.2.1.1 Prepare

在安装之前，需要预先安装Table
2.1中所示的所有依赖环境。其中所有环境的安装请参照相应的安装文档。在安装完成所有依赖环境后，需要进行以下一些操作。

1、在安装完成CUDA后，用户需要确定CUDA版本以及GPU计算能力，最简单的方法就是利用CUDA自带的Sample中的工具deviceQuery来确定。确定CUDA安装目录CUDAInstallPath(例如
/usr/local/cuda),执行如下命令：
```shell
[xxx@localhost ~]cd CUDAInstallPath/samples/1_Utilities/deviceQuery
```
编译deviceQuery程序：
```shell
[xxx@localhost ~]make
```

当成功生成deviceQuery可执行文件后，执行：
```shell
[xxx@localhost ~]./deviceQuery
```

预期产生如下输出(以NVIDIA Tesla V100为例)：

| |
| --------------------- |
|Detected 3 CUDA Capable device(s) |
|Device 0: "Tesla V100-PCIE-32GB"|
|CUDA Driver Version / Runtime Version	10.1 / 10.0|
|CUDA Capability Major/Minor version number:	7.0|
|Total amount of global memory:	32480 MBytes (34058272768 bytes)|
|(80) Multiprocessors, ( 64) CUDA Cores/MP:	|5120 CUDA Cores|
|GPU Max Clock rate:	1380 MHz (1.38 GHz)|
|Memory Clock rate:	877 Mhz|
|Memory Bus Width:	4096-bit|
|L2 Cache Size:	6291456 bytes|
|Maximum Texture Dimension Size (x,y,z)	1D=(131072), 2D=(131072, 65536), 3D=(16384, 16384, 16384)|
|Maximum Layered 1D Texture Size, (num) layers	1D=(32768), 2048 layers|
|Maximum Layered 2D Texture Size, (num) layers	2D=(32768, 32768), 2048 layers|
|Total amount of constant memory:	65536 bytes|
|Total amount of shared memory per block:	49152 bytes|
|Total number of registers available per block:	65536|
|Warp size:	32|
|Maximum number of threads per multiprocessor:	2048|
|Maximum number of threads per block:	1024|
|Max dimension size of a thread block (x,y,z):	(1024, 1024, 64)|
|Max dimension size of a grid size    (x,y,z):	(2147483647, 65535, 65535)|
|Maximum memory pitch:	2147483647 bytes|
|Texture alignment:	512 bytes|
|Concurrent copy and kernel execution:	Yes with 7 copy engine(s)|
|Run time limit on kernels:	No|
|Integrated GPU sharing Host Memory:	No|
|Support host page-locked memory mapping:	Yes|
|Alignment requirement for Surfaces:	Yes|
|Device has ECC support:	Enabled|
|Device supports Unified Addressing (UVA):	Yes|
|Device supports Compute Preemption:	Yes|
|Supports Cooperative Kernel Launch:	Yes|
|Supports MultiDevice Co-op Kernel Launch:	Yes|
|Device PCI Domain ID / Bus ID / location ID:	0 / 24 / 0|


从输出结果可以看出，CUDA Driver版本为CD=10.1，而GPU计算能力为CC=7.0。

2、在安装完成Tiny C之后，需要确定其安装路径，记为PathTinyC
(例如：/home/xxx/tcc-0.9.27)。

3、在安装完成PGI CE Linux之后，需要确定其安装路径，记为PathPGI
(例如：/opt/pgi)。

#### 2.2.1.2正式安装

然后解压安装程序包，以mcpscu_2021_07_02.tar为例，运行如下命令：
[xxx@localhost ~]cp mcpscu_2021_07_02.tar pathToInstall/
[xxx@localhost ~]cd pathToInstall/

此时文件夹pathToInstall中的结构如下:
```
pathToInstall
├── mcpscu_2021_07_02.tar
```
将mcpscu_2021_07_02.tar进行解压：
[xxx@localhost pathToInstall]tar -vxf mcpscu_2021_07_02.tar

此时对应输出为：
```
mcinstall
mcpscu_2021_07_02.tar.bz2
MCSample_2021_07_02/
MCSample_2021_07_02/BatchBox/
MCSample_2021_07_02/BatchBox/Cascade1Box200LU_NBox800.dat
MCSample_2021_07_02/BoxFile.dat
MCSample_2021_07_02/CascadeBox/
MCSample_2021_07_02/CascadeBox/CascadeGenerateCtrlFile.dat
MCSample_2021_07_02/CtrlFile.dat
MCSample_2021_07_02/FromMF973K/
MCSample_2021_07_02/FromMF973K/Config317.dat
MCSample_2021_07_02/GBDIST.dat
MCSample_2021_07_02/ImpF.dat
MCSample_2021_07_02/ImplantDist.dat
MCSample_2021_07_02/IniF.dat
MCSample_2021_07_02/PANDA/
MCSample_2021_07_02/PANDA/HeW100ev100nm.dat
MCSample_2021_07_02/PANDAS/
MCSample_2021_07_02/PANDAS/HeCu100ev100nm.dat
MCSample_2021_07_02/PANDAS/HeCu10Kev200nm.dat
MCSample_2021_07_02/PANDAS/HeCu1Kev100nm.dat
MCSample_2021_07_02/PANDAS/HeW100ev100nm.dat
MCSample_2021_07_02/PANDAS/HeW10Kev200nm.dat
MCSample_2021_07_02/PANDAS/HeW1Kev100nm.dat
MCSample_2021_07_02/SRIM2003/
MCSample_2021_07_02/SRIM2003/HeCu100ev100nmRANGE_3D.txt
MCSample_2021_07_02/SampleSetup.dat
```
解压后的文件夹中的文件目录应该如下：
```
pathToInstall
├── mcinstall
├── mcpscu_2021_07_02.tar
├── mcpscu_2021_07_02.tar.bz2
└── MCSample_2021_07_02
```
根据TinyC的安装位置，修改mcinstall中TINYCCPATH值，具体做法为：使用文本编辑器(例如vim或者gedit)打开mcinstall：
[xxx@localhost pathToInstall]vim mcinstall
按下"i"键进行编辑，修改如下内容为
```
#---- Some Vars that users can modify ----
TINYCCPATH=$PWD/tcc-0.9.27/
```
```
#---- Some Vars that users can modify ----
TINYCCPATH= PathTinyC
```
其中PathTinyC为用户安装TinyC的目录，例如/home/xxx/tcc-0.9.27。

然后按下"：wq"保存并退出。

观察，如果此时mcinstall文件状态为非可执行，需要执行以下命令将其修改为可执行状态：
[xxx@localhost pathToInstall]chmod +x mcinstall
执行安装文件：
[xxx@localhost pathToInstall]./mcinstall
得到如下安装提示信息：
```
mcpscu_2021_07_02/
mcpscu_2021_07_02/TCCLIB/
…
The default CUDA version is:  cuda8.0  ,the computer capability is:  cc35
Reset CUDA Version and computer capability (y/n)?
```
根据之前CUDA安装版本，以及计算能力，如果与默认值(CD=8.0,
CC=35)不匹配，那么，输入y,回车，将会看到：
```
…
The default CUDA version is:  cuda8.0  ,the computer capability is:  cc35
Reset CUDA Version and computer capability (y/n)?y
Please input the CUDA version (xx.x for instance 9.0 , 10.0)
```
此时根据之前得到的CUDA版本，CD=xx.x(例如10.1)，则输入xx.x(例如10.1)
，回车。得到如下结果：
```
The default CUDA version is:  cuda8.0  ,the computer capability is:  cc35
Reset CUDA Version and computer capability (y/n)?y
Please input the CUDA version (xx.x for instance 9.0 , 10.0) 10.1
Please input the computer capability (x.x for instance 2.x , 3.0 , 7.0)
```

此时根据之前得到的GPU计算能力，CC=x.x(例如7.0)，则输入7.0，回车。得到如下结果：
```
The default CUDA version is:  cuda8.0  ,the computer capability is:  cc35
Reset CUDA Version and computer capability (y/n)?y
Please input the CUDA version (xx.x for instance 9.0 , 10.0) 10.1
Please input the computer capability (x.x for instance 2.x , 3.0 , 7.0)7.0
Set these changing to .bashrc file (y/n)?
```
如果不希望将如上变量值写入用户环境变量中(即\~/.bashrc文件中)，那么输入n，回车；(不会影响后续安装运行)

如果决定将如上的变量值写入用户环境变量中(即~/.bashrc文件中)，那么输入y，回车，这样可以提高再次安装程序时的便捷性。这时程序将在用户的环境变量中(即~/.bashrc文件中)产生如下语句：
```
#---The environment for CUDA Driver---
CUDAV=10.1; export CUDAV
CUDAC=7.0; export CUDAC
#---End environment for CUDA Driver---
```
输入y/n之后，产生如下结果：
```
The default CUDA version is:  cuda8.0  ,the computer capability is:  cc35
Reset CUDA Version and computer capability (y/n)?y
Please input the CUDA version (xx.x for instance 9.0 , 10.0) 10.1
Please input the computer capability (x.x for instance 2.x , 3.0 , 7.0)7.0
Set these changing to .bashrc file (y/n)?y
The current CUDA version is:  cuda10.1  ,the computer capability is:  cc70
The default PGI path is /opt/pgi
Reset the PGI path(y/n)?
```
根据之前PGI CE
Linux安装路径PathPGI(例如/opt/pgi)，判断与默认值/opt/pgi是否相符，如果相符，则输入n，回车；

如果不相符，则输入y，回车，得到如下输出结果：
```
The default CUDA version is:  cuda8.0  ,the computer capability is:  cc35
Reset CUDA Version and computer capability (y/n)?y
Please input the CUDA version (xx.x for instance 9.0 , 10.0) 10.1
Please input the computer capability (x.x for instance 2.x , 3.0 , 7.0)7.0
Set these changing to .bashrc file (y/n)?y
The current CUDA version is:  cuda10.1  ,the computer capability is:  cc70
The default PGI path is /opt/pgi
Reset the PGI path(y/n)?y
Please input the path for PGI (for instance /opt/pgi/ or C:PROGRA~1)
```

输入PathPGI(例如/opt/pgi)，回车：
```
The default CUDA version is:  cuda8.0  ,the computer capability is:  cc35
Reset CUDA Version and computer capability (y/n)?y
Please input the CUDA version (xx.x for instance 9.0 , 10.0) 10.1
Please input the computer capability (x.x for instance 2.x , 3.0 , 7.0)7.0
Set these changing to .bashrc file (y/n)?y
The current CUDA version is:  cuda10.1  ,the computer capability is:  cc70
The default PGI path is /opt/pgi
Reset the PGI path(y/n)?y
Please input the path for PGI (for instance /opt/pgi/ or C:PROGRA~1)/opt/pgi
the default pgi version is  17.4
Reset the PGI version (y/n)?
```
根据当前安装PGI CE
Linux版本，决定是否重设PGI版本信息，假设当前PGI版本不为17.4，为19.10，则输入y，回车，得到如下输出结果：

输入当前PGI版本，例如19.10，回车，得到如下结果：
```
The default CUDA version is:  cuda8.0  ,the computer capability is:  cc35
Reset CUDA Version and computer capability (y/n)?y
Please input the CUDA version (xx.x for instance 9.0 , 10.0) 10.1
Please input the computer capability (x.x for instance 2.x , 3.0 , 7.0)7.0
Set these changing to .bashrc file (y/n)?y
The current CUDA version is:  cuda10.1  ,the computer capability is:  cc70
The default PGI path is /opt/pgi
Reset the PGI path(y/n)?y
Please input the path for PGI (for instance /opt/pgi/ or C:PROGRA~1)/opt/pgi
the default pgi version is  17.4
Reset the PGI version (y/n)?y
Please input the PGI version (for instance 19.4 not the 2019.4):
```
如果不希望将如上变量值写入用户环境变量中(即\~/.bashrc文件中)，那么输入n，回车；(不会影响后续安装运行)

如果决定将如上的变量值写入用户环境变量中(即~/.bashrc文件中)，那么输入y，回车，这样可以提高再次安装程序时的便捷性。这时程序将在用户的环境变量中(即~/.bashrc文件中)产生如下语句：

此时，MCPSCU安装完成，将以出现以下内容：

安装完成后，安装目录下生成文件如下：

其中mcanaly和mcapps分别表示两个指向pathToInstall/mcpscu_2021_07_02/MCLIB/sor/ANALYTOOLS和pathToInstall/mcpscu_2021_07_02/MCLIB/sor/APPLICATIONS的软链接。其中包含各种application的主程序。

mcworkspace则为一个文件夹，其中将包含被编译的各种中间文件、打包的库文件，以及链接生成的application可执行程序。

#### 2.2.1.3 Compilation librarys

在完成上述安装操作后，需要编译库文件。进入MCPSCU安装目录：

执行编译命令：

这里\[options\]可以选用如下选项：

假设这里使用默认选项(即创建Release版本库)，执行编译，当出现如下信息则表示编译成功：

编译成功后，在mcworkspace文件夹中，应该生成如下文件夹：

其中LIB文件夹用于存放Release以及Debug版本程序编译的静态库文件。Release版本编译后会生成Release文件夹，Debug版本编译后会生成Debug文件夹。举例来说，Release文件夹中应生成如下库文件：

#### 2.2.1.4 Compilation && Link applications

库文件编译完成后，用户需要根据自身的需求来编译、链接生成应用。MCPSCU中将应用分为两个部分：APPLICATIONS以及ANALYTOOLS。

APPLICATIONS中包含如下文件：

举例来说，若想编译、链接MigrationCoalescence_GPU应用，则进行如下操作：

这里的\[options\]选项应该与调用mcbdlib所使用的\[options\]选项一致。即可选-d，-r(default)，-dc，-rc,
-c。

回车，执行完成之后，在mcworkspace中将新生成APPLICATIONS文件夹，如果之前选择生成Release版本的程序，那么将会在mcworkspace/mcpscu_2021_07_02/APPLICATIONS/Release文件夹下生成可执行程序MigrationCoalescence_GPU.exe。

同理，若想编译、链接MC_GenerateCascadeBox应用，则进行如下操作：

如果之前选择生成Release版本的程序，那么将会生成在mcworkspace/mcpscu_2021_07_02/APPLICATIONS/Release文件夹下生成可执行程序MC_GenerateCascadeBox.exe。

具体每种应用的功能，将在本手册后续内容中进行介绍。

ANALYTOOLS中包含如下文件：

举例来说，若想编译、链接MC_HomogenizeBox工具，则进行如下操作：

这里的\[options\]选项应该与调用mcbdlib所使用的\[options\]选项一致。即可选-d，-r(default)，-dc，-rc,
-c。

回车，执行完成之后，在mcworkspace中将新生成ANALYTOOLS文件夹，如果之前选择生成Release版本的程序，那么将会在mcworkspace/mcpscu_2021_07_02/ANALYTOOLS/Release文件夹下生成可执行程序MC_HomogenizeBox.exe。

同理，若想编译、链接MC_StatisticClusters_Offline工具，则进行如下操作：

如果之前选择生成Release版本的程序，那么将会生成在mcworkspace/mcpscu_2021_07_02/ANALYTOOLS/Release文件夹下生成可执行程序MC_StatisticClusters_Offline.exe。

具体每种工具的功能，将在本手册后续内容中进行介绍。

### 2.2.2 Windows操作系统

#### 2.2.2.1安装准备

在安装之前，需要预先安装Table
2.2中所示的所有依赖环境。其中所有环境的安装请参照相应的安装文档。在安装完成所有依赖环境后，需要进行以下一些操作。

特别需要注意的是，为了使MCPSCU正常使用，安装PGI CE
Win时需要用勾选并安装Cygwin环境。

1、在安装完成CUDA后，用户需要确定CUDA版本以及GPU计算能力，最简单的方法就是利用CUDA自带的工具deviceQuery来确定。确定CUDA安装目录CUDAInstallPath(例如C:`\Program `{=tex}Files`\NVIDIA `{=tex}GPU
Computing
Toolkit`\CUDA`{=tex}`\v1`{=tex}0.1`\extras`{=tex}`\demo`{=tex}\_suite)，进入windows
CMD (Command
Prompt)中，执行如下命令(以安装路径CUDAInstallPath为C:`\Program `{=tex}Files`\NVIDIA `{=tex}GPU
Computing
Toolkit`\CUDA`{=tex}`\v1`{=tex}0.1`\extras`{=tex}`\demo`{=tex}\_suite为例)：

执行deviceQuery.exe：

回车，预期产生如下输出(以NVIDIA GeForce GTX 1660 SUPER为例)：

从输出结果可以看出，CUDA Driver版本为CD=10.1，而GPU计算能力为CC=7.5。

2、双击快捷图标：可以进入Cygwin环境，如果PGI
提供的Cygwin环境的路径为/cygdrive/c/xxx类型，需要修改/etc/fstab中的内容：

按下"i"键进行编辑，将其中的"none /cygdrive cygdrive binary,posix=0,user
0 0"修改为：

none / cygdrive binary,posix=0,user 0 0

然后按下"：wq"保存并退出。

3、在安装完成Tiny C之后，需要确定其安装路径，记为PathTinyC
(例如：/xxx/tcc-0.9.27)。

4、在安装完成PGI CE Linux之后，需要确定其安装路径，记为PathPGI
(例如：C:`\PROGRA`{=tex}~1`\PGI`{=tex}，其中PROGRA~1表示"Program
Files")。

#### 2.2.2.2正式安装

双击快捷图标：进入Cygwin环境，然后解压安装程序包，以mcpscu_2021_07_02.tar为例，运行如下命令：

此时文件夹pathToInstall中的结构如下：

将mcpscu_2021_07_02.tar进行解压：

此时对应输出为：

解压后的文件夹中的文件目录应该如下：

根据TinyC的安装位置，修改mcinstall中TINYCCPATH值，具体做法为：使用文本编辑器(例如vim或者gedit)打开mcinstall：

按下"i"键进行编辑，修改如下内容为

其中PathTinyC为用户安装TinyC的目录，例如/home/xxx/tcc-0.9.27。

然后按下"：wq"保存并退出。

观察，如果此时mcinstall文件状态为非可执行，需要执行以下命令将其修改为可执行状态：

执行安装文件：

得到如下安装提示信息：

根据之前CUDA安装版本，以及计算能力，如果与默认值(CD=8.0,
CC=35)不匹配，那么，输入y,回车，将会看到：

此时根据之前得到的CUDA版本，CD=xx.x(例如10.1)，则输入xx.x(例如10.1)
，回车。得到如下结果：

此时根据之前得到的GPU计算能力，CC=x.x(例如7.5)，则输入7.5，回车。得到如下结果：

如果不希望将如上变量值写入用户环境变量中(即\~/.bashrc文件中)，那么输入n，回车；(不会影响后续安装运行)

如果决定将如上的变量值写入用户环境变量中(即~/.bashrc文件中)，那么输入y，回车，这样可以提高再次安装程序时的便捷性。这时程序将在用户的环境变量中(即~/.bashrc文件中)产生如下语句：

输入y/n之后，产生如下结果：

根据之前PGI CE
Win安装路径PathPGI(例如C:`\PROGRA`{=tex}~1`\PGI`{=tex})，判断与默认值C:`\PROGRA`{=tex}~1`\PGI是否相符`{=tex}，如果相符，则输入n，回车；

如果不相符，则输入y，回车，得到如下输出结果：

输入PathPGI(例如C:`\PROGRA`{=tex}\~1`\PGI`{=tex})，回车：

根据当前安装PGI CE
Win版本，决定是否重设PGI版本信息，假设当前PGI版本不为17.4，为19.10，则输入y，回车，得到如下输出结果：

输入当前PGI版本，例如19.10，回车，得到如下结果：

如果不希望将如上变量值写入用户环境变量中(即\~/.bashrc文件中)，那么输入n，回车；(不会影响后续安装运行)

如果决定将如上的变量值写入用户环境变量中(即~/.bashrc文件中)，那么输入y，回车，这样可以提高再次安装程序时的便捷性。这时程序将在用户的环境变量中(即~/.bashrc文件中)产生如下语句：

此时，MCPSCU安装完成，将以出现以下内容：

安装完成后，安装目录下生成文件如下：

其中mcanaly和mcapps分别表示两个指向pathToInstall/mcpscu_2021_07_02/MCLIB/sor/ANALYTOOLS和pathToInstall/mcpscu_2021_07_02/MCLIB/sor/APPLICATIONS的软链接。其中包含各种application的主程序。

mcworkspace则为一个文件夹，其中将包含被编译的各种中间文件、打包的库文件，以及链接生成的application可执行程序。

#### 2.2.2.3编译库文件

在完成上述安装操作后，需要编译库文件。双击快捷图标：进入Cygwin环境，进入MCPSCU安装目录：

执行编译命令：

这里\[options\]可以选用如下选项：

若出现错误"pgfortran-Error-LLVM code generator
'C:`\PROGRA`{=tex}\~1`\PGI`{=tex}/linux86-64-nollvm/19.10/' is not
available in this installation"，则需要进行如下修复：

打开PGI安装路径如C:`\PROGRA`{=tex}~1`\PGI`{=tex}，进入C:`\PROGRA`{=tex}~1`\PGI`{=tex}`\win64`{=tex}\\19.10`\bin目录中`{=tex}，修改"nativerc"文件，将其中所有"linux86-64-nollvm"改为"win64"。

假设这里使用默认选项(即创建Release版本库)，执行编译，当出现如下信息则表示编译成功：

编译成功后，在mcworkspace文件夹中，应该生成如下文件夹：

其中LIB文件夹用于存放Release以及Debug版本程序编译的静态库文件。Release版本编译后会生成Release文件夹，Debug版本编译后会生成Debug文件夹。举例来说，Release文件夹中应生成如下库文件：

#### 2.2.2.4编译、链接应用

库文件编译完成后，用户需要根据自身的需求来编译、链接生成应用。MCPSCU中将应用分为两个部分：APPLICATIONS以及ANALYTOOLS。

APPLICATIONS中包含如下文件：

举例来说，若想编译、链接MigrationCoalescence_GPU应用，则进行如下操作：

这里的\[options\]选项应该与调用mcbdlib所使用的\[options\]选项一致。即可选-d，-r(default)，-dc，-rc,
-c。

回车，执行完成之后，在mcworkspace中将新生成APPLICATIONS文件夹，如果之前选择生成Release版本的程序，那么将会在mcworkspace/mcpscu_2021_07_02/APPLICATIONS/Release文件夹下生成可执行程序MigrationCoalescence_GPU.exe。

同理，若想编译、链接MC_GenerateCascadeBox应用，则进行如下操作：

如果之前选择生成Release版本的程序，那么将会生成在mcworkspace/mcpscu_2021_07_02/APPLICATIONS/Release文件夹下生成可执行程序MC_GenerateCascadeBox.exe。

具体每种应用的功能，将在本手册后续内容中进行介绍。

ANALYTOOLS中包含如下文件：

举例来说，若想编译、链接MC_HomogenizeBox工具，则进行如下操作：

这里的\[options\]选项应该与调用mcbdlib所使用的\[options\]选项一致。即可选-d，-r(default)，-dc，-rc,
-c。

回车，执行完成之后，在mcworkspace中将新生成ANALYTOOLS文件夹，如果之前选择生成Release版本的程序，那么将会在mcworkspace/mcpscu_2021_07_02/ANALYTOOLS/Release文件夹下生成可执行程序MC_HomogenizeBox.exe。

同理，若想编译、链接MC_StatisticClusters_Offline工具，则进行如下操作：

如果之前选择生成Release版本的程序，那么将会生成在mcworkspace/mcpscu_2021_07_02/ANALYTOOLS/Release文件夹下生成可执行程序MC_StatisticClusters_Offline.exe。

具体每种工具的功能，将在本手册后续内容中进行介绍。