# Introdutions
## 1 Software Introduction

MCPSCU (Monte Carlo package for Sichuan University) is a GPU (Graphics Processing Unit) parallelized migration–coalescence kinetic Monte Carlo simulation package.

MCPSCU was jointly developed by graduate student Lei Zhai and researcher Qing Hou from the Radiation Physics and Medical Physics Research Group, Institute of Nuclear Science and Technology, Sichuan University, starting from 2017. It is mainly applied to the simulation of the migration–coalescence evolution of diffusing objects (such as impurities and defects) in materials. Up to now, MCPSCU has undergone multiple iterations and version upgrades. Through continuous studies of the physical processes involved in the MCPSCU program, several efficient computational strategies proposed by the developers have been incorporated, resulting in a complete high-performance computational package. Currently, MCPSCU has been successfully applied by researchers to simulate the evolution of helium bubbles, displacement defects, and defect clusters in materials.

![alt text](image.png)
Figure 1.1 Structural framework diagram of the MCPSCU program.

MCPSCU is mainly developed using Fortran 95/2003. According to the characteristics of the Fortran 95/2003 language, MCPSCU adopts an object-oriented programming approach. A multi-level compilation dependency structure consisting of programs, libraries, and modules is used to achieve a well-organized program architecture.

To enable GPU parallelization, MCPSCU uses the CUDA Fortran parallel programming framework provided by PGI to implement kernel drivers, parallel kernel business logic, multi-thread organization, and multi-thread scheduling. In addition, to facilitate user-defined properties of diffusing objects and reaction events, MCPSCU uses Tiny C as a dynamic scripting interface for user interaction. Figure 1.1 shows the structural framework of the MCPSCU program.

MCPSCU has undergone multiple stages of development. Around 2008, researchers Qing Hou, Yulu Zhou, Chaoqiong Ma, Renshun Li, and others verified the feasibility of using migration–coalescence kinetic Monte Carlo methods to simulate helium bubble growth in materials and developed a simple CPU serial demo program. Since 2017, graduate student Lei Zhai and researcher Qing Hou have completely redeveloped MCPSCU to achieve systematic, object-oriented, extensible, hierarchical, and parallel development. Through multiple functional upgrades and version iterations, MCPSCU has evolved into a highly stable software package capable of high-concurrency operation, mainly under Linux environments.

### 1.1 Version history of MCPSCU
| Version           | Event |
| ----------------- | ---- |
| mcpscu_Origin     | A simple CPU serial demo program  |
| mcpscu_v_xxx      | Complete program reconstruction, initiating object-oriented, extensible, hierarchical, and parallel development|
| **Version**       | **Event**  |
| mcpscu_2017_09_30 | Reconstructed the program and implemented GPU parallelization for neighbor list calculation|
| mcpscu_2017_10_12 | Implemented parallel migration calculations of diffusors on the GPU side|
| mcpscu_2017_10_29 | Generated the GPU-side coalescence list before coalescence events|
| mcpscu_2017_11_09 | Proposed using the nearest neighbor list instead of the cut-off range neighbor list |
| mcpscu_2017_11_12 | Implemented the GPU-side neighbor list generator|
| mcpscu_2017_11_22 | Completed diffusor coalescence operations on the GPU side|
| mcpscu_2017_12_10 | Implemented the **Multiple-Box in one run** functionality|
| mcpscu_2018_03_21 | Added compilation and execution support under Linux (CentOS) environments|
| mcpscu_2018_05_14 | Added continuous implantation functionality|
| mcpscu_2019_02_15 | Refactored the code and encapsulated all data and methods using an object-oriented approach; added support for grain boundary settings and continuous particle introduction functionality|
| mcpscu_2019_02_15 | Standardized user input files and control parameters; enabled user-defined defect types and reaction events; completed dynamic mapping between user-defined defect object models and user-defined reaction event models on both CPU and GPU platforms|
| mcpscu_2019_02_16 | Divided program memory into three ranges: **virtual range**, **expand range**, and **used range**, improving dynamic memory expansion capability |
| mcpscu_2019_02_20 | Added support for running the program under the CYGWIN environment|
| mcpscu_2019_03_20 | Added multiple offline analysis and statistical functions|
| mcpscu_2021_07_02 | Added the capability to parse molecular dynamics input configurations, enabling analysis and construction of initial defect configurations through multiple approaches; enabled parsing and reading of user-defined multi-batch diffusor configuration folders, including storage of imported information and dynamic invocation during implantation |

Note: Version control is based on the GitLab local server and Git distributed version control system.