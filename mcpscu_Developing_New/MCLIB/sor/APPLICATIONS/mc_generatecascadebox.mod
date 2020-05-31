V30 :0x4 mc_generatecascadebox
78 /home/zhail/mcpscu_2020_04_29/MCLIB/sor/APPLICATIONS/MC_GenerateCascadeBox.F90 S624 0
05/23/2020  16:25:48
use mclib_global public 0 direct
use migcoale_typedef_simrecord public 0 direct
use mclib_typedef_neighbor_list public 0 indirect
use mclib_typedef_clustersinfo_cpu public 0 indirect
use mclib_typedef_basicrecord public 0 indirect
use miniutilities public 0 indirect
use msm_typedef_inputpaser public 0 indirect
use model_ecr_cpu public 0 indirect
use iso_c_binding public 0 indirect
use mclib_typedef_diffusorsvalue public 0 indirect
use mclib_typedef_diffusorproplist public 0 indirect
use mclib_typedef_reactionsvalue public 0 indirect
use mclib_typedef_reactionproplist public 0 indirect
use mclib_typedef_simulationctrlparam public 0 indirect
use mclib_typedef_geometry public 0 indirect
use mclib_typedef_simulationboxarray public 0 direct
use migcoale_addondata_host public 0 direct
use msm_constants public 0 indirect
use mclib_constants public 0 indirect
use mclib_typedef_acluster public 0 indirect
use model_typedef_atomslist public 0 indirect
use mclib_utilities_former public 0 indirect
use mclib_utilities public 0 direct
use rand32_module public 0 direct
use rand32seedlib_module public 0 direct
enduse
D 58 18 51
D 64 18 141
D 66 21 64 1 3 200 0 0 0 0 0
 0 200 3 3 200 200
D 71 21 58 1 3 200 0 0 0 0 0
 0 200 3 3 200 200
D 174 24 1119 272 1118 7
D 185 20 174
D 269 24 1394 32 1393 7
D 275 18 2
D 282 24 1409 96 1408 7
D 291 20 282
D 323 24 1488 12 1487 3
D 332 24 1493 48 1492 3
D 368 24 1519 152 1518 7
D 377 20 368
D 733 24 1857 208 1854 7
D 821 24 1857 208 1854 7
D 827 24 1944 384 1942 7
D 909 24 2007 136 2006 7
D 915 21 6 1 3 200 0 0 0 0 0
 0 200 3 3 200 200
D 918 21 6 1 3 200 0 0 0 0 0
 0 200 3 3 200 200
D 921 21 7 1 3 200 0 0 0 0 0
 0 200 3 3 200 200
D 927 24 2021 224 2020 7
D 942 24 2041 560 2038 7
D 978 20 7
D 980 20 7
D 982 20 7
D 984 20 7
D 986 20 7
D 991 24 2085 144 2084 3
D 1014 24 2097 352 2096 7
D 1020 21 6 1 3 200 0 0 0 0 0
 0 200 3 3 200 200
D 1041 24 2378 272 2377 7
D 1047 18 2
D 1054 20 1041
D 1059 24 2386 328 2385 7
D 1070 20 1041
D 1184 24 2606 8 2605 7
D 1193 24 2609 8 2608 7
D 1300 18 2
D 1304 24 2678 176 2677 7
D 1310 21 9 1 3 49 0 0 0 0 0
 0 49 3 3 49 49
D 1337 24 2732 216 2731 7
D 1457 18 2
D 1461 24 2678 176 2677 7
D 1467 21 9 1 3 49 0 0 0 0 0
 0 49 3 3 49 49
D 1476 24 2851 240 2850 7
D 1485 20 1476
D 1612 18 2
D 1616 24 2934 128 2933 7
D 1643 24 2980 216 2979 7
D 1763 18 2
D 1767 24 2934 128 2933 7
D 1779 24 3104 192 3103 7
D 1788 20 1779
D 1869 24 2378 272 2377 7
D 1875 18 2
D 1889 24 3213 1904 3212 7
D 1898 21 6 1 3 18 0 0 0 0 0
 0 18 3 3 18 18
D 1904 21 6 1 3 49 0 0 0 0 0
 0 49 3 3 49 49
D 1913 21 16 1 3 200 0 0 0 0 0
 0 200 3 3 200 200
D 1927 20 1869
D 1929 20 1869
D 1931 20 1889
D 2027 18 2
D 2062 24 3414 40 3413 7
D 2071 20 2062
D 2076 24 3436 408 3435 7
D 2082 21 9 1 3 18 0 0 0 0 0
 0 18 3 3 18 18
D 2145 24 1394 32 1393 7
D 2151 18 2
D 2153 24 1409 96 1408 7
D 2250 24 1857 208 1854 7
D 2256 24 1944 384 1942 7
D 2262 24 2007 136 2006 7
D 2268 21 6 1 3 200 0 0 0 0 0
 0 200 3 3 200 200
D 2271 21 6 1 3 200 0 0 0 0 0
 0 200 3 3 200 200
D 2274 21 7 1 3 200 0 0 0 0 0
 0 200 3 3 200 200
D 2277 24 2021 224 2020 7
D 2283 24 2041 560 2038 7
D 2289 20 7
D 2291 20 7
D 2293 20 7
D 2295 20 7
D 2297 20 7
D 2337 24 2732 216 2731 7
D 2354 24 2851 240 2850 7
D 2370 24 2980 216 2979 7
D 2384 24 3104 192 3103 7
D 2394 18 2
D 2404 24 3436 408 3435 7
D 2410 21 9 1 3 18 0 0 0 0 0
 0 18 3 3 18 18
D 2413 24 3574 2320 3573 7
D 2419 21 9 2 15 207 0 0 0 0 0
 0 49 3 3 49 49
 0 18 49 3 18 18
D 2422 21 9 1 3 49 0 0 0 0 0
 0 49 3 3 49 49
D 2425 21 9 1 3 49 0 0 0 0 0
 0 49 3 3 49 49
D 2437 20 2153
D 2439 20 2354
D 2441 20 2384
D 3011 24 2085 144 2084 3
D 3017 24 2097 352 2096 7
D 3023 21 6 1 3 200 0 0 0 0 0
 0 200 3 3 200 200
D 3309 24 3938 416 3937 7
D 3673 18 141
D 3675 18 2
D 3737 24 4203 24 4202 7
D 3743 21 9 1 3 49 0 0 0 0 0
 0 49 3 3 49 49
D 3746 21 6 1 0 3 0 0 0 0 0
 0 3 0 3 3 0
D 3749 24 4206 2040 4205 7
D 3755 21 9 1 1405 1404 0 1 0 0 1
 1399 1402 1403 1399 1402 1400
D 3758 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3761 21 9 1 1414 1413 0 1 0 0 1
 1408 1411 1412 1408 1411 1409
D 3764 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3767 21 9 1 1423 1422 0 1 0 0 1
 1417 1420 1421 1417 1420 1418
D 3770 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3773 21 9 1 1432 1431 0 1 0 0 1
 1426 1429 1430 1426 1429 1427
D 3776 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3779 21 9 1 1441 1440 0 1 0 0 1
 1435 1438 1439 1435 1438 1436
D 3782 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3785 21 9 1 1450 1449 0 1 0 0 1
 1444 1447 1448 1444 1447 1445
D 3788 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3791 21 9 1 1459 1458 0 1 0 0 1
 1453 1456 1457 1453 1456 1454
D 3794 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3797 21 9 1 1468 1467 0 1 0 0 1
 1462 1465 1466 1462 1465 1463
D 3800 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3803 21 9 1 1477 1476 0 1 0 0 1
 1471 1474 1475 1471 1474 1472
D 3806 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3809 21 9 1 1486 1485 0 1 0 0 1
 1480 1483 1484 1480 1483 1481
D 3812 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3815 21 9 1 1495 1494 0 1 0 0 1
 1489 1492 1493 1489 1492 1490
D 3818 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3821 21 9 1 1504 1503 0 1 0 0 1
 1498 1501 1502 1498 1501 1499
D 3824 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3827 21 9 1 1513 1512 0 1 0 0 1
 1507 1510 1511 1507 1510 1508
D 3830 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3833 21 9 1 1522 1521 0 1 0 0 1
 1516 1519 1520 1516 1519 1517
D 3836 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3839 21 9 1 1531 1530 0 1 0 0 1
 1525 1528 1529 1525 1528 1526
D 3842 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3845 21 9 1 1540 1539 0 1 0 0 1
 1534 1537 1538 1534 1537 1535
D 3848 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3851 21 9 1 1549 1548 0 1 0 0 1
 1543 1546 1547 1543 1546 1544
D 3854 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3857 21 6 1 1558 1557 0 1 0 0 1
 1552 1555 1556 1552 1555 1553
D 3860 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3863 21 6 1 1567 1566 0 1 0 0 1
 1561 1564 1565 1561 1564 1562
D 3866 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3869 21 6 1 1576 1575 0 1 0 0 1
 1570 1573 1574 1570 1573 1571
D 3872 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3915 21 6 1 0 3 0 0 0 0 0
 0 3 0 3 3 0
D 3918 21 9 1 1586 1592 0 1 0 0 1
 1587 1590 1591 1587 1590 1588
D 3921 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3924 21 9 1 1594 1600 0 1 0 0 1
 1595 1598 1599 1595 1598 1596
D 3927 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3930 21 6 1 1602 1608 0 1 0 0 1
 1603 1606 1607 1603 1606 1604
D 3933 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3936 21 6 1 1610 1616 0 1 0 0 1
 1611 1614 1615 1611 1614 1612
D 3939 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3942 18 307
D 3944 18 307
D 3946 21 3737 1 1618 1624 0 1 0 0 1
 1619 1622 1623 1619 1622 1620
D 3949 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3952 21 6 1 1626 1632 0 1 0 0 1
 1627 1630 1631 1627 1630 1628
D 3955 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3958 18 307
D 3960 21 3737 1 1634 1640 0 1 0 0 1
 1635 1638 1639 1635 1638 1636
D 3963 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3966 21 6 1 1642 1648 0 1 0 0 1
 1643 1646 1647 1643 1646 1644
D 3969 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3972 21 6 1 1650 1656 0 1 0 0 1
 1651 1654 1655 1651 1654 1652
D 3975 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3978 21 6 1 1658 1664 0 1 0 0 1
 1659 1662 1663 1659 1662 1660
D 3981 21 6 1 0 311 0 0 0 0 0
 0 311 0 3 311 0
D 3984 21 6 1 0 3 0 0 0 0 0
 0 3 0 3 3 0
S 624 24 0 0 0 6 1 0 5011 10005 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 mc_generatecascadebox
S 633 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 634 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 635 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 645 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 646 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 647 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 100 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 662 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1044740494 -500134854 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 769 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 770 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 6 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 771 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 20 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 773 3 0 0 0 3673 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 5864 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 20 41 43 54 49 56 45 46 52 45 45 20 20 20 20 20 20 20 20 20 20
S 774 3 0 0 0 3673 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 5885 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 20 41 43 54 49 56 45 49 4e 47 42 20 20 20 20 20 20 20 20 20 20
S 775 3 0 0 0 3673 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 5906 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 20 4f 55 54 5f 44 45 53 54 52 4f 59 20 20 20 20 20 20 20 20 20
S 776 3 0 0 0 3673 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 5927 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 20 45 58 50 5f 44 45 53 54 52 4f 20 20 20 20 20 20 20 20 20 20
S 777 3 0 0 0 3673 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 5948 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 20 4d 49 53 5f 44 45 53 54 52 4f 59 20 20 20 20 20 20 20 20 20
S 778 3 0 0 0 3673 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 5969 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 20 41 42 53 4f 52 42 45 44 20 20 20 20 20 20 20 20 20 20 20 20
S 779 3 0 0 0 3673 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 5990 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 20 41 4e 4e 49 48 49 4c 41 54 45 20 20 20 20 20 20 20 20 20 20
R 853 7 58 mclib_constants p_cstatu$ac
S 888 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 256 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 1111 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 18 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 1112 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 13 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 1113 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 14 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 1114 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 17 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 1118 25 4 mclib_utilities_former strlist
R 1119 5 5 mclib_utilities_former thevalue strlist
R 1120 5 6 mclib_utilities_former listcount strlist
R 1121 5 7 mclib_utilities_former next strlist
R 1123 5 9 mclib_utilities_former next$p strlist
R 1125 14 11 mclib_utilities_former copystrlistfromother$tbp
R 1133 5 19 mclib_utilities_former cleanstrlist$0 strlist
R 1134 5 20 mclib_utilities_former =$1 strlist
R 1135 5 21 mclib_utilities_former clean_strlist$tbp$2 strlist
R 1136 5 22 mclib_utilities_former getstrlist_count$tbp$3 strlist
R 1137 5 23 mclib_utilities_former getvaluebystrlistindex$tbp$4 strlist
R 1138 5 24 mclib_utilities_former appendarray_strlist$tbp$5 strlist
R 1139 5 25 mclib_utilities_former appendone_strlist$tbp$6 strlist
R 1140 5 26 mclib_utilities_former copystrlistfromother$tbp$7 strlist
S 1391 3 0 0 0 3675 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 9955 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 0
R 1393 25 2 model_typedef_atomslist atom
R 1394 5 3 model_typedef_atomslist m_id atom
R 1395 5 4 model_typedef_atomslist m_symbol atom
R 1396 5 5 model_typedef_atomslist m_elementindex atom
R 1397 5 6 model_typedef_atomslist m_atommass atom
R 1398 5 7 model_typedef_atomslist m_volum atom
R 1399 14 8 model_typedef_atomslist copyatomfromother$tbp
R 1403 5 12 model_typedef_atomslist clean_atom$0 atom
R 1404 5 13 model_typedef_atomslist =$1 mdstatistic
R 1405 5 14 model_typedef_atomslist cleanatom$tbp$2 atom
R 1406 5 15 model_typedef_atomslist copyatomfromother$tbp$3 atom
R 1408 25 17 model_typedef_atomslist atomslist
R 1409 5 18 model_typedef_atomslist m_atom atomslist
R 1410 5 19 model_typedef_atomslist m_atomsnumber atomslist
R 1411 5 20 model_typedef_atomslist m_listcount atomslist
R 1412 5 21 model_typedef_atomslist next atomslist
R 1414 5 23 model_typedef_atomslist next$td atomslist
R 1415 5 24 model_typedef_atomslist next$p atomslist
R 1420 14 29 model_typedef_atomslist copyatomslistfromother$tbp
R 1424 5 33 model_typedef_atomslist clean_atomslist$4 atomslist
R 1425 5 34 model_typedef_atomslist =$5 mdstatistic
R 1426 5 35 model_typedef_atomslist getsymbolbyindex$tbp$6 atomslist
R 1427 5 36 model_typedef_atomslist findindexbysymbol$tbp$7 atomslist
R 1428 5 37 model_typedef_atomslist copyatomslistfromother$tbp$8 atomslist
R 1429 5 38 model_typedef_atomslist cleanatomslist$tbp$9 atomslist
R 1430 5 39 model_typedef_atomslist appendone$tbp$10 atomslist
R 1431 5 40 model_typedef_atomslist get_listcount$tbp$11 atomslist
R 1487 25 5 mclib_typedef_acluster single_atomssetrange
R 1488 5 6 mclib_typedef_acluster m_id single_atomssetrange
R 1489 5 7 mclib_typedef_acluster m_na_from single_atomssetrange
R 1490 5 8 mclib_typedef_acluster m_na_to single_atomssetrange
R 1492 25 10 mclib_typedef_acluster atomssetrange
R 1493 5 11 mclib_typedef_acluster m_setsrange atomssetrange
R 1497 5 15 mclib_typedef_acluster atomssetrange2clusterlist$tbp$0 atomssetrange
R 1498 5 16 mclib_typedef_acluster permutationatomssetrange2clusterlist$tbp$1 atomssetrange
R 1499 5 17 mclib_typedef_acluster releasesetsrange$tbp$2 atomssetrange
R 1511 14 29 mclib_typedef_acluster copyclusterfromother$tbp
R 1518 25 36 mclib_typedef_acluster aclusterlist
R 1519 5 37 mclib_typedef_acluster thecluster aclusterlist
R 1520 5 38 mclib_typedef_acluster quantififyvalue aclusterlist
R 1521 5 39 mclib_typedef_acluster listcount aclusterlist
R 1522 5 40 mclib_typedef_acluster next aclusterlist
R 1524 5 42 mclib_typedef_acluster next$p aclusterlist
R 1528 14 46 mclib_typedef_acluster copyclusterslistfromother$tbp
R 1531 5 49 mclib_typedef_acluster cleanclusterlist$6 aclusterlist
R 1532 5 50 mclib_typedef_acluster =$7 mdstatistic
R 1533 5 51 mclib_typedef_acluster clean_clusterlist$tbp$8 aclusterlist
R 1534 5 52 mclib_typedef_acluster copyclusterslistfromother$tbp$9 aclusterlist
R 1535 5 53 mclib_typedef_acluster getlist_count$tbp$10 aclusterlist
R 1536 5 54 mclib_typedef_acluster appendotherclusterlist$tbp$11 aclusterlist
R 1537 5 55 mclib_typedef_acluster appendonecluster$tbp$12 aclusterlist
R 1854 25 1 mclib_typedef_neighbor_list neighbor_list
R 1857 5 4 mclib_typedef_neighbor_list m_indi neighbor_list
R 1858 5 5 mclib_typedef_neighbor_list m_indi$sd neighbor_list
R 1859 5 6 mclib_typedef_neighbor_list m_indi$p neighbor_list
R 1860 5 7 mclib_typedef_neighbor_list m_indi$o neighbor_list
R 1863 5 10 mclib_typedef_neighbor_list m_kvois neighbor_list
R 1864 5 11 mclib_typedef_neighbor_list m_kvois$sd neighbor_list
R 1865 5 12 mclib_typedef_neighbor_list m_kvois$p neighbor_list
R 1866 5 13 mclib_typedef_neighbor_list m_kvois$o neighbor_list
R 1868 5 15 mclib_typedef_neighbor_list nlupdatecount_host neighbor_list
R 1870 5 17 mclib_typedef_neighbor_list clear_neighbor_list$0 neighbor_list
R 1871 5 18 mclib_typedef_neighbor_list =$2 neighbor_list
R 1872 5 19 mclib_typedef_neighbor_list release$tbp$3 neighbor_list
R 1873 5 20 mclib_typedef_neighbor_list increaseonenlupdatecount_host$tbp$4 neighbor_list
R 1874 5 21 mclib_typedef_neighbor_list setnlupdatecount_host$tbp$5 neighbor_list
R 1875 5 22 mclib_typedef_neighbor_list getnlupdatecount_host$tbp$6 neighbor_list
R 1876 5 23 mclib_typedef_neighbor_list copyneighborlist$tbp$7 neighbor_list
R 1877 5 24 mclib_typedef_neighbor_list dumplicateneighborlist$tbp$8 neighbor_list
R 1878 5 25 mclib_typedef_neighbor_list resizeneighborlist$tbp$9 neighbor_list
R 1879 5 26 mclib_typedef_neighbor_list getneighborlistsize$tbp$10 neighbor_list
R 1880 5 27 mclib_typedef_neighbor_list getstatus_neighbor_list_allocated$tbp$11 neighbor_list
R 1881 5 28 mclib_typedef_neighbor_list allocateneighbor_list$tbp$12 neighbor_list
R 1888 14 35 mclib_typedef_neighbor_list copyneighborlist$tbp
R 1942 25 1 mclib_typedef_clustersinfo_cpu clustersinfo_cpu
R 1944 5 3 mclib_typedef_clustersinfo_cpu m_clusters clustersinfo_cpu
R 1945 5 4 mclib_typedef_clustersinfo_cpu m_clusters$sd clustersinfo_cpu
R 1946 5 5 mclib_typedef_clustersinfo_cpu m_clusters$p clustersinfo_cpu
R 1947 5 6 mclib_typedef_clustersinfo_cpu m_clusters$o clustersinfo_cpu
R 1949 5 8 mclib_typedef_clustersinfo_cpu m_list clustersinfo_cpu
R 1951 5 10 mclib_typedef_clustersinfo_cpu m_activeindex clustersinfo_cpu
R 1952 5 11 mclib_typedef_clustersinfo_cpu m_activeindex$sd clustersinfo_cpu
R 1953 5 12 mclib_typedef_clustersinfo_cpu m_activeindex$p clustersinfo_cpu
R 1954 5 13 mclib_typedef_clustersinfo_cpu m_activeindex$o clustersinfo_cpu
R 1956 14 15 mclib_typedef_clustersinfo_cpu copy_clustersinfo_cpu$tbp
R 1958 5 17 mclib_typedef_clustersinfo_cpu cleanclustersinfo_cpu$0 clustersinfo_cpu
R 1959 5 18 mclib_typedef_clustersinfo_cpu =$4 clustersinfo_cpu
R 1960 5 19 mclib_typedef_clustersinfo_cpu clean$tbp$5 clustersinfo_cpu
R 1961 5 20 mclib_typedef_clustersinfo_cpu getmemoryconsuming_oneclusterinfo_cpu$tbp$6 clustersinfo_cpu
R 1962 5 21 mclib_typedef_clustersinfo_cpu copy_clustersinfo_cpu$tbp$7 clustersinfo_cpu
R 1963 5 22 mclib_typedef_clustersinfo_cpu dumplicateclustersinfo_cpu$tbp$8 clustersinfo_cpu
R 1964 5 23 mclib_typedef_clustersinfo_cpu getclustersinfo_arraysize$tbp$9 clustersinfo_cpu
R 1965 5 24 mclib_typedef_clustersinfo_cpu allocateclustersinfo_cpu$tbp$10 clustersinfo_cpu
S 2005 3 0 0 0 16 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16
R 2006 25 1 mclib_typedef_basicrecord boxstatis
R 2007 5 2 mclib_typedef_basicrecord nc0 boxstatis
R 2008 5 3 mclib_typedef_basicrecord nc boxstatis
R 2009 5 4 mclib_typedef_basicrecord na boxstatis
R 2010 5 5 mclib_typedef_basicrecord ncdumpadded boxstatis
R 2011 5 6 mclib_typedef_basicrecord avenearestspefreeclusters boxstatis
R 2012 5 7 mclib_typedef_basicrecord avenearestspegbclusters boxstatis
R 2013 14 8 mclib_typedef_basicrecord copyboxstatisfromother$tbp
R 2014 5 9 mclib_typedef_basicrecord cleanboxstatis$0 boxstatis
R 2015 5 10 mclib_typedef_basicrecord =$2 boxstatis
R 2016 5 11 mclib_typedef_basicrecord copyboxstatisfromother$tbp$3 boxstatis
R 2017 5 12 mclib_typedef_basicrecord clean$tbp$4 boxstatis
R 2018 5 13 mclib_typedef_basicrecord init$tbp$5 boxstatis
R 2020 25 15 mclib_typedef_basicrecord boxesbasicstatistic
R 2021 5 16 mclib_typedef_basicrecord boxesstatis_integral boxesbasicstatistic
R 2023 5 18 mclib_typedef_basicrecord boxesstatis_single boxesbasicstatistic
R 2024 5 19 mclib_typedef_basicrecord boxesstatis_single$sd boxesbasicstatistic
R 2025 5 20 mclib_typedef_basicrecord boxesstatis_single$p boxesbasicstatistic
R 2026 5 21 mclib_typedef_basicrecord boxesstatis_single$o boxesbasicstatistic
R 2030 14 25 mclib_typedef_basicrecord copyboxesbasicstatisticfromother$tbp
R 2032 5 27 mclib_typedef_basicrecord cleanboxesbasicstatistic$6 boxesbasicstatistic
R 2033 5 28 mclib_typedef_basicrecord =$8 boxesbasicstatistic
R 2034 5 29 mclib_typedef_basicrecord copyboxesbasicstatisticfromother$tbp$9 boxesbasicstatistic
R 2035 5 30 mclib_typedef_basicrecord clean$tbp$10 boxesbasicstatistic
R 2036 5 31 mclib_typedef_basicrecord init$tbp$11 boxesbasicstatistic
R 2038 25 33 mclib_typedef_basicrecord boxesinfo
R 2041 5 36 mclib_typedef_basicrecord seactindexbox boxesinfo
R 2042 5 37 mclib_typedef_basicrecord seactindexbox$sd boxesinfo
R 2043 5 38 mclib_typedef_basicrecord seactindexbox$p boxesinfo
R 2044 5 39 mclib_typedef_basicrecord seactindexbox$o boxesinfo
R 2048 5 43 mclib_typedef_basicrecord seusedindexbox boxesinfo
R 2049 5 44 mclib_typedef_basicrecord seusedindexbox$sd boxesinfo
R 2050 5 45 mclib_typedef_basicrecord seusedindexbox$p boxesinfo
R 2051 5 46 mclib_typedef_basicrecord seusedindexbox$o boxesinfo
R 2055 5 50 mclib_typedef_basicrecord sevirtualindexbox boxesinfo
R 2056 5 51 mclib_typedef_basicrecord sevirtualindexbox$sd boxesinfo
R 2057 5 52 mclib_typedef_basicrecord sevirtualindexbox$p boxesinfo
R 2058 5 53 mclib_typedef_basicrecord sevirtualindexbox$o boxesinfo
R 2062 5 57 mclib_typedef_basicrecord seexpdindexbox boxesinfo
R 2063 5 58 mclib_typedef_basicrecord seexpdindexbox$sd boxesinfo
R 2064 5 59 mclib_typedef_basicrecord seexpdindexbox$p boxesinfo
R 2065 5 60 mclib_typedef_basicrecord seexpdindexbox$o boxesinfo
R 2069 5 64 mclib_typedef_basicrecord seaddedclustersboxes boxesinfo
R 2070 5 65 mclib_typedef_basicrecord seaddedclustersboxes$sd boxesinfo
R 2071 5 66 mclib_typedef_basicrecord seaddedclustersboxes$p boxesinfo
R 2072 5 67 mclib_typedef_basicrecord seaddedclustersboxes$o boxesinfo
R 2076 14 71 mclib_typedef_basicrecord copyboxesinfofromother$tbp
R 2078 5 73 mclib_typedef_basicrecord cleanboxesinfo$12 boxesinfo
R 2079 5 74 mclib_typedef_basicrecord =$13 boxesinfo
R 2080 5 75 mclib_typedef_basicrecord copyboxesinfofromother$tbp$14 boxesinfo
R 2081 5 76 mclib_typedef_basicrecord clean$tbp$15 boxesinfo
R 2082 5 77 mclib_typedef_basicrecord init$tbp$16 boxesinfo
R 2084 25 79 mclib_typedef_basicrecord runningrecord
R 2085 5 80 mclib_typedef_basicrecord lastrecordoutprofiletime runningrecord
R 2086 5 81 mclib_typedef_basicrecord stoprunningflag runningrecord
R 2087 5 82 mclib_typedef_basicrecord start_clock runningrecord
R 2088 5 83 mclib_typedef_basicrecord end_clock runningrecord
R 2089 5 84 mclib_typedef_basicrecord start_datetime runningrecord
R 2090 5 85 mclib_typedef_basicrecord end_datetime runningrecord
R 2092 5 87 mclib_typedef_basicrecord isstoppedrunning$tbp$17 runningrecord
R 2093 5 88 mclib_typedef_basicrecord stoprunning$tbp$18 runningrecord
R 2094 5 89 mclib_typedef_basicrecord initrunningrecord$tbp$19 runningrecord
R 2096 25 91 mclib_typedef_basicrecord simulationrecord
R 2097 5 92 mclib_typedef_basicrecord running_record simulationrecord
R 2098 5 93 mclib_typedef_basicrecord simulaitonsteps simulationrecord
R 2099 5 94 mclib_typedef_basicrecord simulationpatch simulationrecord
R 2100 5 95 mclib_typedef_basicrecord simulationtimes simulationrecord
R 2101 5 96 mclib_typedef_basicrecord timestep simulationrecord
R 2102 5 97 mclib_typedef_basicrecord timesections simulationrecord
R 2103 5 98 mclib_typedef_basicrecord lastupdatestatistime simulationrecord
R 2104 5 99 mclib_typedef_basicrecord lastupdatenltime simulationrecord
R 2105 5 100 mclib_typedef_basicrecord lastupdatenl_nc0 simulationrecord
R 2106 5 101 mclib_typedef_basicrecord recordncbeforesweepout_integal simulationrecord
R 2109 5 104 mclib_typedef_basicrecord recordncbeforesweepout_singlebox simulationrecord
R 2110 5 105 mclib_typedef_basicrecord recordncbeforesweepout_singlebox$sd simulationrecord
R 2111 5 106 mclib_typedef_basicrecord recordncbeforesweepout_singlebox$p simulationrecord
R 2112 5 107 mclib_typedef_basicrecord recordncbeforesweepout_singlebox$o simulationrecord
R 2114 5 109 mclib_typedef_basicrecord lastrecordoutconfigtime simulationrecord
R 2115 5 110 mclib_typedef_basicrecord outputindex simulationrecord
R 2116 5 111 mclib_typedef_basicrecord triggerfocusedtimepoints simulationrecord
R 2120 5 115 mclib_typedef_basicrecord thedefproc$tbp$20 simulationrecord
R 2121 5 116 mclib_typedef_basicrecord getstatustriggerfocusedtimepoints$tbp$21 simulationrecord
R 2122 5 117 mclib_typedef_basicrecord turnofftriggerfocusedtimepoints$tbp$22 simulationrecord
R 2123 5 118 mclib_typedef_basicrecord turnontriggerfocusedtimepoints$tbp$23 simulationrecord
R 2124 5 119 mclib_typedef_basicrecord increaseoneoutputindex$tbp$24 simulationrecord
R 2125 5 120 mclib_typedef_basicrecord setoutputindex$tbp$25 simulationrecord
R 2126 5 121 mclib_typedef_basicrecord getoutputindex$tbp$26 simulationrecord
R 2127 5 122 mclib_typedef_basicrecord setlastrecordoutconfigtime$tbp$27 simulationrecord
R 2128 5 123 mclib_typedef_basicrecord getlastrecordoutconfigtime$tbp$28 simulationrecord
R 2129 5 124 mclib_typedef_basicrecord recordnc_forsweepout$tbp$29 simulationrecord
R 2130 5 125 mclib_typedef_basicrecord setlastupdatenlnc0$tbp$30 simulationrecord
R 2131 5 126 mclib_typedef_basicrecord getlastupdatenlnc0$tbp$31 simulationrecord
R 2132 5 127 mclib_typedef_basicrecord setlastupdatenltime$tbp$32 simulationrecord
R 2133 5 128 mclib_typedef_basicrecord getlastupdatenltime$tbp$33 simulationrecord
R 2134 5 129 mclib_typedef_basicrecord setlastupdatestatistime$tbp$34 simulationrecord
R 2135 5 130 mclib_typedef_basicrecord getlastupdatestatistime$tbp$35 simulationrecord
R 2136 5 131 mclib_typedef_basicrecord increaseonetimesection$tbp$36 simulationrecord
R 2137 5 132 mclib_typedef_basicrecord gettimesections$tbp$37 simulationrecord
R 2138 5 133 mclib_typedef_basicrecord settimesections$tbp$38 simulationrecord
R 2139 5 134 mclib_typedef_basicrecord getsimupatch$tbp$39 simulationrecord
R 2140 5 135 mclib_typedef_basicrecord setsimupatch$tbp$40 simulationrecord
R 2141 5 136 mclib_typedef_basicrecord gettimesteps$tbp$41 simulationrecord
R 2142 5 137 mclib_typedef_basicrecord settimesteps$tbp$42 simulationrecord
R 2143 5 138 mclib_typedef_basicrecord addsimutimes$tbp$43 simulationrecord
R 2144 5 139 mclib_typedef_basicrecord getsimutimes$tbp$44 simulationrecord
R 2145 5 140 mclib_typedef_basicrecord setsimutimes$tbp$45 simulationrecord
R 2146 5 141 mclib_typedef_basicrecord increaseonesimustep$tbp$46 simulationrecord
R 2147 5 142 mclib_typedef_basicrecord getsimusteps$tbp$47 simulationrecord
R 2148 5 143 mclib_typedef_basicrecord setsimusteps$tbp$48 simulationrecord
R 2149 5 144 mclib_typedef_basicrecord initsimulationrecord$tbp$49 simulationrecord
R 2377 25 1 msm_typedef_inputpaser statementlist
R 2378 5 2 msm_typedef_inputpaser line statementlist
R 2379 5 3 msm_typedef_inputpaser this statementlist
R 2380 5 4 msm_typedef_inputpaser next statementlist
R 2382 5 6 msm_typedef_inputpaser next$p statementlist
R 2385 25 9 msm_typedef_inputpaser inputstatements
R 2386 5 10 msm_typedef_inputpaser filename inputstatements
R 2387 5 11 msm_typedef_inputpaser stag inputstatements
R 2388 5 12 msm_typedef_inputpaser stat inputstatements
R 2390 5 14 msm_typedef_inputpaser stat$p inputstatements
R 2605 25 6 iso_c_binding c_ptr
R 2606 5 7 iso_c_binding val c_ptr
R 2608 25 9 iso_c_binding c_funptr
R 2609 5 10 iso_c_binding val c_funptr
R 2643 6 44 iso_c_binding c_null_ptr$ac
R 2645 6 46 iso_c_binding c_null_funptr$ac
R 2646 26 47 iso_c_binding ==
R 2648 26 49 iso_c_binding !=
S 2676 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 2677 25 1 mclib_typedef_diffusorsvalue readeddiffusorvalue
R 2678 5 2 mclib_typedef_diffusorsvalue symbol readeddiffusorvalue
R 2679 5 3 mclib_typedef_diffusorsvalue diffusorvaluetype_free readeddiffusorvalue
R 2680 5 4 mclib_typedef_diffusorsvalue diffusecoefficient_free_value readeddiffusorvalue
R 2681 5 5 mclib_typedef_diffusorsvalue prefactor_free readeddiffusorvalue
R 2682 5 6 mclib_typedef_diffusorsvalue prefactorparameter_free readeddiffusorvalue
R 2683 5 7 mclib_typedef_diffusorsvalue actenergy_free readeddiffusorvalue
R 2684 5 8 mclib_typedef_diffusorsvalue diffusedirectiontype readeddiffusorvalue
R 2685 5 9 mclib_typedef_diffusorsvalue diffusedirection readeddiffusorvalue
R 2686 5 10 mclib_typedef_diffusorsvalue ecrvaluetype_free readeddiffusorvalue
R 2687 5 11 mclib_typedef_diffusorsvalue ecr_free readeddiffusorvalue
R 2688 5 12 mclib_typedef_diffusorsvalue diffusorvaluetype_ingb readeddiffusorvalue
R 2689 5 13 mclib_typedef_diffusorsvalue diffusecoefficient_ingb_value readeddiffusorvalue
R 2690 5 14 mclib_typedef_diffusorsvalue prefactor_ingb readeddiffusorvalue
R 2691 5 15 mclib_typedef_diffusorsvalue prefactorparameter_ingb readeddiffusorvalue
R 2692 5 16 mclib_typedef_diffusorsvalue actenergy_ingb readeddiffusorvalue
R 2693 5 17 mclib_typedef_diffusorsvalue ecrvaluetype_ingb readeddiffusorvalue
R 2694 5 18 mclib_typedef_diffusorsvalue ecr_ingb readeddiffusorvalue
R 2695 14 19 mclib_typedef_diffusorsvalue copyreadeddiffusorvaluefromother$tbp
R 2697 5 21 mclib_typedef_diffusorsvalue cleanreadeddiffusorvalue$0 readeddiffusorvalue
R 2698 5 22 mclib_typedef_diffusorsvalue =$2 readeddiffusorvalue
R 2699 5 23 mclib_typedef_diffusorsvalue convert2diffusorvalue$tbp$3 readeddiffusorvalue
R 2700 5 24 mclib_typedef_diffusorsvalue copyreadeddiffusorvaluefromother$tbp$4 readeddiffusorvalue
R 2719 14 43 mclib_typedef_diffusorsvalue copydiffusorvaluefromother$tbp
R 2727 14 51 mclib_typedef_diffusorsvalue copydiffusortypeentityfromother$tbp
R 2731 25 55 mclib_typedef_diffusorsvalue diffusortypesmap
R 2732 5 56 mclib_typedef_diffusorsvalue maxdividegroups_singleelement diffusortypesmap
R 2733 5 57 mclib_typedef_diffusorsvalue mapbitlength diffusortypesmap
R 2734 5 58 mclib_typedef_diffusorsvalue maplength diffusortypesmap
R 2737 5 61 mclib_typedef_diffusorsvalue singleatomsdividearrays diffusortypesmap
R 2738 5 62 mclib_typedef_diffusorsvalue singleatomsdividearrays$sd diffusortypesmap
R 2739 5 63 mclib_typedef_diffusorsvalue singleatomsdividearrays$p diffusortypesmap
R 2740 5 64 mclib_typedef_diffusorsvalue singleatomsdividearrays$o diffusortypesmap
R 2743 5 67 mclib_typedef_diffusorsvalue typesentities diffusortypesmap
R 2744 5 68 mclib_typedef_diffusorsvalue typesentities$sd diffusortypesmap
R 2745 5 69 mclib_typedef_diffusorsvalue typesentities$p diffusortypesmap
R 2746 5 70 mclib_typedef_diffusorsvalue typesentities$o diffusortypesmap
R 2751 14 75 mclib_typedef_diffusorsvalue copydiffusortypesmapfromother$tbp
R 2753 5 77 mclib_typedef_diffusorsvalue cleandiffusortypesmap$10 diffusortypesmap
R 2754 5 78 mclib_typedef_diffusorsvalue =$11 diffusortypesmap
R 2755 5 79 mclib_typedef_diffusorsvalue clean$tbp$12 diffusortypesmap
R 2756 5 80 mclib_typedef_diffusorsvalue copydiffusortypesmapfromother$tbp$13 diffusortypesmap
R 2757 5 81 mclib_typedef_diffusorsvalue getindexfor$tbp$14 diffusortypesmap
R 2758 5 82 mclib_typedef_diffusorsvalue hash$tbp$15 diffusortypesmap
R 2759 5 83 mclib_typedef_diffusorsvalue getcode$tbp$16 diffusortypesmap
R 2760 5 84 mclib_typedef_diffusorsvalue constructor$tbp$17 diffusortypesmap
R 2761 5 85 mclib_typedef_diffusorsvalue get$tbp$18 diffusortypesmap
R 2762 5 86 mclib_typedef_diffusorsvalue put$tbp$19 diffusortypesmap
R 2850 25 2 mclib_typedef_diffusorproplist readdiffusorproplist
R 2851 5 3 mclib_typedef_diffusorproplist diffusor readdiffusorproplist
R 2852 5 4 mclib_typedef_diffusorproplist next readdiffusorproplist
R 2854 5 6 mclib_typedef_diffusorproplist next$td readdiffusorproplist
R 2855 5 7 mclib_typedef_diffusorproplist next$p readdiffusorproplist
R 2857 5 9 mclib_typedef_diffusorproplist listcount readdiffusorproplist
R 2862 14 14 mclib_typedef_diffusorproplist copyreaddiffusorproplistfromother$tbp
R 2866 5 18 mclib_typedef_diffusorproplist cleanreaddiffusorproplist$0 readdiffusorproplist
R 2867 5 19 mclib_typedef_diffusorproplist =$4 readdiffusorproplist
R 2868 5 20 mclib_typedef_diffusorproplist printoutcheckingresult$tbp$5 readdiffusorproplist
R 2869 5 21 mclib_typedef_diffusorproplist clean_readdiffusorproplist$tbp$6 readdiffusorproplist
R 2870 5 22 mclib_typedef_diffusorproplist copyreaddiffusorproplistfromother$tbp$7 readdiffusorproplist
R 2871 5 23 mclib_typedef_diffusorproplist getlist_count$tbp$8 readdiffusorproplist
R 2872 5 24 mclib_typedef_diffusorproplist converttodiffusorstypesmap$tbp$9 readdiffusorproplist
R 2873 5 25 mclib_typedef_diffusorproplist getreaddiffusorbylistindex$tbp$10 readdiffusorproplist
R 2874 5 26 mclib_typedef_diffusorproplist appendarray_readdiffusorproplist$tbp$11 readdiffusorproplist
R 2875 5 27 mclib_typedef_diffusorproplist appendone_readdiffusorproplist$tbp$12 readdiffusorproplist
S 2931 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 -1074790400 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 2932 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 9 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 2933 25 1 mclib_typedef_reactionsvalue readreactionpair
R 2934 5 2 mclib_typedef_reactionsvalue subjectsymbol readreactionpair
R 2935 5 3 mclib_typedef_reactionsvalue objectsymbol readreactionpair
R 2936 5 4 mclib_typedef_reactionsvalue reactioncoefficienttype readreactionpair
R 2937 5 5 mclib_typedef_reactionsvalue reactioncoefficient_value readreactionpair
R 2938 5 6 mclib_typedef_reactionsvalue prefactor readreactionpair
R 2939 5 7 mclib_typedef_reactionsvalue actenergy readreactionpair
R 2940 5 8 mclib_typedef_reactionsvalue productiontype readreactionpair
R 2941 5 9 mclib_typedef_reactionsvalue element_subject readreactionpair
R 2942 5 10 mclib_typedef_reactionsvalue element_object readreactionpair
R 2943 5 11 mclib_typedef_reactionsvalue ecrvaluetype readreactionpair
R 2944 5 12 mclib_typedef_reactionsvalue ecr readreactionpair
R 2945 14 13 mclib_typedef_reactionsvalue copyreadedreactionpairfromother$tbp
R 2947 5 15 mclib_typedef_reactionsvalue cleanreadedreactionpair$0 readreactionpair
R 2948 5 16 mclib_typedef_reactionsvalue =$2 readreactionpair
R 2949 5 17 mclib_typedef_reactionsvalue convert2reactionvalue$tbp$3 readreactionpair
R 2950 5 18 mclib_typedef_reactionsvalue copyreadedreactionpairfromother$tbp$4 readreactionpair
R 2962 14 30 mclib_typedef_reactionsvalue copyreactionvaluefromother$tbp
R 2973 14 41 mclib_typedef_reactionsvalue copyreactionentityfromother$tbp
R 2979 25 47 mclib_typedef_reactionsvalue reactionsmap
R 2980 5 48 mclib_typedef_reactionsvalue maxdividegroups_singleelement reactionsmap
R 2981 5 49 mclib_typedef_reactionsvalue mapbitlength reactionsmap
R 2982 5 50 mclib_typedef_reactionsvalue maplength reactionsmap
R 2985 5 53 mclib_typedef_reactionsvalue singleatomsdividearrays reactionsmap
R 2986 5 54 mclib_typedef_reactionsvalue singleatomsdividearrays$sd reactionsmap
R 2987 5 55 mclib_typedef_reactionsvalue singleatomsdividearrays$p reactionsmap
R 2988 5 56 mclib_typedef_reactionsvalue singleatomsdividearrays$o reactionsmap
R 2991 5 59 mclib_typedef_reactionsvalue recordsentities reactionsmap
R 2992 5 60 mclib_typedef_reactionsvalue recordsentities$sd reactionsmap
R 2993 5 61 mclib_typedef_reactionsvalue recordsentities$p reactionsmap
R 2994 5 62 mclib_typedef_reactionsvalue recordsentities$o reactionsmap
R 2999 14 67 mclib_typedef_reactionsvalue copyreactionsmapfromother$tbp
R 3001 5 69 mclib_typedef_reactionsvalue cleanreactionsmap$12 reactionsmap
R 3002 5 70 mclib_typedef_reactionsvalue =$13 reactionsmap
R 3003 5 71 mclib_typedef_reactionsvalue clean$tbp$14 reactionsmap
R 3004 5 72 mclib_typedef_reactionsvalue copyreactionsmapfromother$tbp$15 reactionsmap
R 3005 5 73 mclib_typedef_reactionsvalue getindexfor$tbp$16 reactionsmap
R 3006 5 74 mclib_typedef_reactionsvalue hash$tbp$17 reactionsmap
R 3007 5 75 mclib_typedef_reactionsvalue getcode$tbp$18 reactionsmap
R 3008 5 76 mclib_typedef_reactionsvalue constructor$tbp$19 reactionsmap
R 3009 5 77 mclib_typedef_reactionsvalue get$tbp$20 reactionsmap
R 3010 5 78 mclib_typedef_reactionsvalue put$tbp$21 reactionsmap
R 3103 25 1 mclib_typedef_reactionproplist readreactionproplist
R 3104 5 2 mclib_typedef_reactionproplist reaction readreactionproplist
R 3105 5 3 mclib_typedef_reactionproplist next readreactionproplist
R 3107 5 5 mclib_typedef_reactionproplist next$td readreactionproplist
R 3108 5 6 mclib_typedef_reactionproplist next$p readreactionproplist
R 3110 5 8 mclib_typedef_reactionproplist listcount readreactionproplist
R 3115 14 13 mclib_typedef_reactionproplist copyreadreactionproplistfromother$tbp
R 3120 5 18 mclib_typedef_reactionproplist cleanreadreactionproplist$0 readreactionproplist
R 3121 5 19 mclib_typedef_reactionproplist =$4 readreactionproplist
R 3122 5 20 mclib_typedef_reactionproplist whetherfreediffusion$tbp$5 readreactionproplist
R 3123 5 21 mclib_typedef_reactionproplist printoutcheckingresult$tbp$6 readreactionproplist
R 3124 5 22 mclib_typedef_reactionproplist clean_readreactionproplist$tbp$7 readreactionproplist
R 3125 5 23 mclib_typedef_reactionproplist copyreadreactionproplistfromother$tbp$8 readreactionproplist
R 3126 5 24 mclib_typedef_reactionproplist getlist_count$tbp$9 readreactionproplist
R 3127 5 25 mclib_typedef_reactionproplist converttoreactionsmap$tbp$10 readreactionproplist
R 3128 5 26 mclib_typedef_reactionproplist getreadreactionbylistindex$tbp$11 readreactionproplist
R 3129 5 27 mclib_typedef_reactionproplist appendarray_readreactionproplist$tbp$12 readreactionproplist
R 3130 5 28 mclib_typedef_reactionproplist appendone_readreactionproplist$tbp$13 readreactionproplist
S 3196 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 43434 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 3197 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 54454532 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 3198 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 2048 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 3199 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1081262080 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 3200 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1025986740 359966101 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 3201 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 3000 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 3202 3 0 0 0 8 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 1008981770 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 8
S 3203 3 0 0 0 16 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 -1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16
S 3204 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 -1 -1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 3212 25 8 mclib_typedef_simulationctrlparam simulationctrlparam
R 3213 5 9 mclib_typedef_simulationctrlparam restartat simulationctrlparam
R 3214 5 10 mclib_typedef_simulationctrlparam multibox simulationctrlparam
R 3215 5 11 mclib_typedef_simulationctrlparam totalbox simulationctrlparam
R 3216 5 12 mclib_typedef_simulationctrlparam indepbox simulationctrlparam
R 3217 5 13 mclib_typedef_simulationctrlparam randseed simulationctrlparam
R 3218 5 14 mclib_typedef_simulationctrlparam period simulationctrlparam
R 3219 5 15 mclib_typedef_simulationctrlparam neighborcalway simulationctrlparam
R 3220 5 16 mclib_typedef_simulationctrlparam maxneighbornum simulationctrlparam
R 3221 5 17 mclib_typedef_simulationctrlparam cutregionextend simulationctrlparam
R 3222 5 18 mclib_typedef_simulationctrlparam neighborupdatestrategy simulationctrlparam
R 3223 5 19 mclib_typedef_simulationctrlparam neighborupdate simulationctrlparam
R 3224 5 20 mclib_typedef_simulationctrlparam temp simulationctrlparam
R 3225 5 21 mclib_typedef_simulationctrlparam tkb simulationctrlparam
R 3226 5 22 mclib_typedef_simulationctrlparam implantsectid simulationctrlparam
R 3227 5 23 mclib_typedef_simulationctrlparam termtflag simulationctrlparam
R 3228 5 24 mclib_typedef_simulationctrlparam termtvalue simulationctrlparam
R 3229 5 25 mclib_typedef_simulationctrlparam nfocusedtimepoint simulationctrlparam
R 3231 5 27 mclib_typedef_simulationctrlparam focusedtimepoints simulationctrlparam
R 3232 5 28 mclib_typedef_simulationctrlparam focusedtimepoints$sd simulationctrlparam
R 3233 5 29 mclib_typedef_simulationctrlparam focusedtimepoints$p simulationctrlparam
R 3234 5 30 mclib_typedef_simulationctrlparam focusedtimepoints$o simulationctrlparam
R 3236 5 32 mclib_typedef_simulationctrlparam updatetstepstrategy simulationctrlparam
R 3237 5 33 mclib_typedef_simulationctrlparam fixedtimestepvalue simulationctrlparam
R 3238 5 34 mclib_typedef_simulationctrlparam enlagetstepscale simulationctrlparam
R 3239 5 35 mclib_typedef_simulationctrlparam tupdatestatisflag simulationctrlparam
R 3240 5 36 mclib_typedef_simulationctrlparam tupdatestatisvalue simulationctrlparam
R 3241 5 37 mclib_typedef_simulationctrlparam outputconfflag simulationctrlparam
R 3242 5 38 mclib_typedef_simulationctrlparam outputconfvalue simulationctrlparam
R 3243 5 39 mclib_typedef_simulationctrlparam outputconfcontent simulationctrlparam
R 3244 5 40 mclib_typedef_simulationctrlparam outputconf_sweepout simulationctrlparam
R 3245 5 41 mclib_typedef_simulationctrlparam outputscflag simulationctrlparam
R 3246 5 42 mclib_typedef_simulationctrlparam outputscvalue_integralbox simulationctrlparam
R 3247 5 43 mclib_typedef_simulationctrlparam outputscvalue_eachbox simulationctrlparam
R 3248 5 44 mclib_typedef_simulationctrlparam outputfuncsflag simulationctrlparam
R 3249 5 45 mclib_typedef_simulationctrlparam outputfuncsvalue simulationctrlparam
R 3250 5 46 mclib_typedef_simulationctrlparam outputswapflag simulationctrlparam
R 3251 5 47 mclib_typedef_simulationctrlparam outputswapvalue simulationctrlparam
R 3252 5 48 mclib_typedef_simulationctrlparam addondata simulationctrlparam
R 3254 5 50 mclib_typedef_simulationctrlparam addondata$p simulationctrlparam
R 3256 5 52 mclib_typedef_simulationctrlparam modeldata simulationctrlparam
R 3258 5 54 mclib_typedef_simulationctrlparam modeldata$p simulationctrlparam
R 3260 5 56 mclib_typedef_simulationctrlparam inputfilepath simulationctrlparam
R 3261 5 57 mclib_typedef_simulationctrlparam inputfileshortname simulationctrlparam
R 3262 5 58 mclib_typedef_simulationctrlparam iniconfig simulationctrlparam
R 3263 5 59 mclib_typedef_simulationctrlparam impfile simulationctrlparam
R 3264 5 60 mclib_typedef_simulationctrlparam outfilepath simulationctrlparam
R 3265 5 61 mclib_typedef_simulationctrlparam restartcfg simulationctrlparam
R 3266 5 62 mclib_typedef_simulationctrlparam startjob simulationctrlparam
R 3267 5 63 mclib_typedef_simulationctrlparam endjob simulationctrlparam
R 3268 5 64 mclib_typedef_simulationctrlparam jobstep simulationctrlparam
R 3269 5 65 mclib_typedef_simulationctrlparam starttsection simulationctrlparam
R 3270 5 66 mclib_typedef_simulationctrlparam endtsection simulationctrlparam
R 3271 5 67 mclib_typedef_simulationctrlparam tsectionstep simulationctrlparam
R 3272 5 68 mclib_typedef_simulationctrlparam startcfg simulationctrlparam
R 3273 5 69 mclib_typedef_simulationctrlparam endcfg simulationctrlparam
R 3274 5 70 mclib_typedef_simulationctrlparam cfgstep simulationctrlparam
R 3275 5 71 mclib_typedef_simulationctrlparam startbox simulationctrlparam
R 3276 5 72 mclib_typedef_simulationctrlparam endbox simulationctrlparam
R 3277 5 73 mclib_typedef_simulationctrlparam boxstep simulationctrlparam
R 3278 5 74 mclib_typedef_simulationctrlparam freediffusion simulationctrlparam
R 3279 5 75 mclib_typedef_simulationctrlparam next simulationctrlparam
R 3281 5 77 mclib_typedef_simulationctrlparam next$p simulationctrlparam
R 3284 14 80 mclib_typedef_simulationctrlparam copyfromother$tbp
R 3300 5 96 mclib_typedef_simulationctrlparam clean$0 simulationctrlparam
R 3301 5 97 mclib_typedef_simulationctrlparam =$2 mdstatistic
R 3302 5 98 mclib_typedef_simulationctrlparam load_modeldatastatments$tbp$3 simulationctrlparam
R 3303 5 99 mclib_typedef_simulationctrlparam load_addondatastatments$tbp$4 simulationctrlparam
R 3304 5 100 mclib_typedef_simulationctrlparam load_ctrl_timestep$tbp$5 simulationctrlparam
R 3305 5 101 mclib_typedef_simulationctrlparam load_ctrl_implant$tbp$6 simulationctrlparam
R 3306 5 102 mclib_typedef_simulationctrlparam load_ctrl_neighborlist$tbp$7 simulationctrlparam
R 3307 5 103 mclib_typedef_simulationctrlparam load_ctrl_boundary$tbp$8 simulationctrlparam
R 3308 5 104 mclib_typedef_simulationctrlparam load_ctrl_temperature$tbp$9 simulationctrlparam
R 3309 5 105 mclib_typedef_simulationctrlparam load_ctrl_sectionparameter$tbp$10 simulationctrlparam
R 3310 5 106 mclib_typedef_simulationctrlparam load_ctrl_analyparameter$tbp$11 simulationctrlparam
R 3311 5 107 mclib_typedef_simulationctrlparam load_ctrl_commparameter$tbp$12 simulationctrlparam
R 3312 5 108 mclib_typedef_simulationctrlparam print_ctrlparameters$tbp$13 simulationctrlparam
R 3313 5 109 mclib_typedef_simulationctrlparam load_ctrl_parameters$tbp$14 simulationctrlparam
R 3314 5 110 mclib_typedef_simulationctrlparam defaultvalue_ctrlparam$tbp$15 simulationctrlparam
R 3315 5 111 mclib_typedef_simulationctrlparam cleansimulationctrlparam$tbp$16 simulationctrlparam
R 3316 5 112 mclib_typedef_simulationctrlparam copyfromother$tbp$17 simulationctrlparam
R 3317 5 113 mclib_typedef_simulationctrlparam get_p$tbp$18 simulationctrlparam
R 3318 5 114 mclib_typedef_simulationctrlparam appendone_simulationctrlparam$tbp$19 simulationctrlparam
R 3409 14 8 mclib_typedef_geometry copyseedsformother$tbp
R 3413 25 12 mclib_typedef_geometry grainseedlist
R 3414 5 13 mclib_typedef_geometry seed grainseedlist
R 3415 5 14 mclib_typedef_geometry next grainseedlist
R 3417 5 16 mclib_typedef_geometry next$p grainseedlist
R 3419 5 18 mclib_typedef_geometry m_count grainseedlist
R 3425 14 24 mclib_typedef_geometry copygrainseedlisfromother$tbp
R 3427 5 26 mclib_typedef_geometry destroygrainseedlist$2 grainseedlist
R 3428 5 27 mclib_typedef_geometry =$4 mdstatistic
R 3429 5 28 mclib_typedef_geometry copygrainseedlisfromother$tbp$5 grainseedlist
R 3430 5 29 mclib_typedef_geometry cleangrainseedlist$tbp$6 grainseedlist
R 3431 5 30 mclib_typedef_geometry converttoarray$tbp$7 grainseedlist
R 3432 5 31 mclib_typedef_geometry appendoneseed$tbp$8 grainseedlist
R 3433 5 32 mclib_typedef_geometry appendone$tbp$9 grainseedlist
R 3435 25 34 mclib_typedef_geometry grainboundary
R 3436 5 35 mclib_typedef_geometry gbinittype grainboundary
R 3437 5 36 mclib_typedef_geometry gbinitsimple_strategy grainboundary
R 3438 5 37 mclib_typedef_geometry cutoff grainboundary
R 3439 5 38 mclib_typedef_geometry grainnum grainboundary
R 3440 5 39 mclib_typedef_geometry seedsdistini grainboundary
R 3441 5 40 mclib_typedef_geometry seedsdistsd grainboundary
R 3442 5 41 mclib_typedef_geometry gvolumini grainboundary
R 3443 5 42 mclib_typedef_geometry gvolumsd grainboundary
R 3444 5 43 mclib_typedef_geometry gbcfgfilename grainboundary
R 3446 5 45 mclib_typedef_geometry grainseeds grainboundary
R 3447 5 46 mclib_typedef_geometry grainseeds$sd grainboundary
R 3448 5 47 mclib_typedef_geometry grainseeds$p grainboundary
R 3449 5 48 mclib_typedef_geometry grainseeds$o grainboundary
R 3460 14 59 mclib_typedef_geometry copygrainboundaryfromother$tbp
R 3462 5 61 mclib_typedef_geometry cleangrainboundary$10 grainboundary
R 3463 5 62 mclib_typedef_geometry =$11 mdstatistic
R 3464 5 63 mclib_typedef_geometry copygrainboundaryfromother$tbp$12 grainboundary
R 3465 5 64 mclib_typedef_geometry clean_grainboundary$tbp$13 grainboundary
R 3466 5 65 mclib_typedef_geometry grainbelongsto$tbp$14 grainboundary
R 3467 5 66 mclib_typedef_geometry rescalegrainboundary$tbp$15 grainboundary
R 3468 5 67 mclib_typedef_geometry constructgrainboundary_specialdistfromextefunc$tbp$16 grainboundary
R 3469 5 68 mclib_typedef_geometry constructgrainboundary_specialdistfromfile$tbp$17 grainboundary
R 3470 5 69 mclib_typedef_geometry constructgrainboundary_simple_bygvolumctl$tbp$18 grainboundary
R 3471 5 70 mclib_typedef_geometry constructgrainboundary_simple_bygseedctl$tbp$19 grainboundary
R 3472 5 71 mclib_typedef_geometry constructgrainboundary_simple$tbp$20 grainboundary
R 3473 5 72 mclib_typedef_geometry constructgrainboundary$tbp$21 grainboundary
S 3564 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1046535061 1218006876 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
R 3573 25 9 mclib_typedef_simulationboxarray simulationboxes
R 3574 5 10 mclib_typedef_simulationboxarray m_diffusortypesmap simulationboxes
R 3575 5 11 mclib_typedef_simulationboxarray m_reactionsmap simulationboxes
R 3576 5 12 mclib_typedef_simulationboxarray m_clustersinfo_cpu simulationboxes
R 3577 5 13 mclib_typedef_simulationboxarray m_grainboundary simulationboxes
R 3578 5 14 mclib_typedef_simulationboxarray latticelength simulationboxes
R 3579 5 15 mclib_typedef_simulationboxarray boxboundary simulationboxes
R 3580 5 16 mclib_typedef_simulationboxarray boxsize simulationboxes
R 3581 5 17 mclib_typedef_simulationboxarray hboxsize simulationboxes
R 3582 5 18 mclib_typedef_simulationboxarray boxvolum simulationboxes
R 3583 5 19 mclib_typedef_simulationboxarray matrixatom simulationboxes
R 3584 5 20 mclib_typedef_simulationboxarray atoms_list simulationboxes
R 3586 5 22 mclib_typedef_simulationboxarray atoms_list$td simulationboxes
R 3587 5 23 mclib_typedef_simulationboxarray atoms_list$p simulationboxes
R 3589 5 25 mclib_typedef_simulationboxarray m_boxesinfo simulationboxes
R 3590 5 26 mclib_typedef_simulationboxarray m_boxesbasicstatistic simulationboxes
R 3591 5 27 mclib_typedef_simulationboxarray readdiffusorprop_list simulationboxes
R 3593 5 29 mclib_typedef_simulationboxarray readdiffusorprop_list$td simulationboxes
R 3594 5 30 mclib_typedef_simulationboxarray readdiffusorprop_list$p simulationboxes
R 3596 5 32 mclib_typedef_simulationboxarray readreactionprop_list simulationboxes
R 3598 5 34 mclib_typedef_simulationboxarray readreactionprop_list$td simulationboxes
R 3599 5 35 mclib_typedef_simulationboxarray readreactionprop_list$p simulationboxes
R 3630 14 66 mclib_typedef_simulationboxarray copysimulationboxesfromother$tbp
R 3632 5 68 mclib_typedef_simulationboxarray destorysimulationboxes$0 simulationboxes
R 3633 5 69 mclib_typedef_simulationboxarray clean$tbp$1 simulationboxes
R 3634 5 70 mclib_typedef_simulationboxarray =$10 mdstatistic
R 3635 5 71 mclib_typedef_simulationboxarray copysimulationboxesfromother$tbp$11 simulationboxes
R 3636 5 72 mclib_typedef_simulationboxarray expandclustersinfor_cpu$tbpg$12 simulationboxes
R 3637 5 73 mclib_typedef_simulationboxarray expandclustersinfor_cpu$tbpg$13 simulationboxes
R 3638 5 74 mclib_typedef_simulationboxarray expandclustersinfor_cpu_boxbybox$tbp$14 simulationboxes
R 3639 5 75 mclib_typedef_simulationboxarray expandclustersinfor_cpu_equalnum$tbp$15 simulationboxes
R 3640 5 76 mclib_typedef_simulationboxarray doputin_fromdistribution$tbp$16 simulationboxes
R 3641 5 77 mclib_typedef_simulationboxarray putin_spmf_outcfg_format18_distribution$tbp$17 simulationboxes
R 3642 5 78 mclib_typedef_simulationboxarray putin_spmf_outcfg_format18$tbp$18 simulationboxes
R 3643 5 79 mclib_typedef_simulationboxarray putin_mf_outcfg_format18_distribution$tbp$19 simulationboxes
R 3644 5 80 mclib_typedef_simulationboxarray putin_mf_outcfg_format18$tbp$20 simulationboxes
R 3645 5 81 mclib_typedef_simulationboxarray putin_okmc_outcfg_format18$tbp$21 simulationboxes
R 3646 5 82 mclib_typedef_simulationboxarray putin_okmc_outcfg_format18_simrecord$tbp$22 simulationboxes
R 3647 5 83 mclib_typedef_simulationboxarray putincfg$tbp$23 simulationboxes
R 3648 5 84 mclib_typedef_simulationboxarray putoutcfg$tbp$24 simulationboxes
R 3649 5 85 mclib_typedef_simulationboxarray putouttofile$tbp$25 simulationboxes
R 3650 5 86 mclib_typedef_simulationboxarray getoneboxbasicstatistic_allstatu_cpu$tbp$26 simulationboxes
R 3651 5 87 mclib_typedef_simulationboxarray getboxesbasicstatistic_allstatu_cpu$tbp$27 simulationboxes
R 3652 5 88 mclib_typedef_simulationboxarray sweepunactivememory_cpu$tbp$28 simulationboxes
R 3653 5 89 mclib_typedef_simulationboxarray rescaleboxes_cpu$tbp$29 simulationboxes
R 3654 5 90 mclib_typedef_simulationboxarray initsimulationbox$tbp$30 simulationboxes
R 3655 5 91 mclib_typedef_simulationboxarray load_gb_specialdistfromextefunc$tbp$31 simulationboxes
R 3656 5 92 mclib_typedef_simulationboxarray load_gb_specialdistfromfile$tbp$32 simulationboxes
R 3657 5 93 mclib_typedef_simulationboxarray load_gb_simple_distribution_bygvolumctl$tbp$33 simulationboxes
R 3658 5 94 mclib_typedef_simulationboxarray load_gb_simple_distribution_bygseedctl$tbp$34 simulationboxes
R 3659 5 95 mclib_typedef_simulationboxarray load_gb_simple$tbp$35 simulationboxes
R 3660 5 96 mclib_typedef_simulationboxarray load_box_grainboundary$tbp$36 simulationboxes
R 3661 5 97 mclib_typedef_simulationboxarray loadreactionsfromscript$tbp$37 simulationboxes
R 3662 5 98 mclib_typedef_simulationboxarray loadonereaction$tbp$38 simulationboxes
R 3663 5 99 mclib_typedef_simulationboxarray loadreactions$tbp$39 simulationboxes
R 3664 5 100 mclib_typedef_simulationboxarray loaddiffusorsvaluefromscript$tbp$40 simulationboxes
R 3665 5 101 mclib_typedef_simulationboxarray reslovediffusorsvaluefromcscript$tbp$41 simulationboxes
R 3666 5 102 mclib_typedef_simulationboxarray loadonediffusors$tbp$42 simulationboxes
R 3667 5 103 mclib_typedef_simulationboxarray loaddiffusorsvalue$tbp$43 simulationboxes
R 3668 5 104 mclib_typedef_simulationboxarray load_box_diffusors$tbp$44 simulationboxes
R 3669 5 105 mclib_typedef_simulationboxarray load_onesecton_atomdefine$tbp$45 simulationboxes
R 3670 5 106 mclib_typedef_simulationboxarray load_box_atomsdefine$tbp$46 simulationboxes
R 3671 5 107 mclib_typedef_simulationboxarray load_box_shape$tbp$47 simulationboxes
R 3672 5 108 mclib_typedef_simulationboxarray print_parameter_simulationboxes$tbp$48 simulationboxes
R 3673 5 109 mclib_typedef_simulationboxarray loadparameter_simulationboxes$tbp$49 simulationboxes
R 3674 5 110 mclib_typedef_simulationboxarray defaultvaluesimulationboxes$tbp$50 simulationboxes
R 3918 26 3 mclib_global =
O 3918 1 4367
R 3937 25 1 migcoale_typedef_simrecord migcoalclusterrecord
R 3938 5 2 migcoale_typedef_simrecord simulationrecord migcoalclusterrecord
R 3939 5 3 migcoale_typedef_simrecord startimplanttime migcoalclusterrecord
R 3940 5 4 migcoale_typedef_simrecord implantedentities migcoalclusterrecord
R 3941 5 5 migcoale_typedef_simrecord lastrecordimplantnum migcoalclusterrecord
R 3942 5 6 migcoale_typedef_simrecord ncut migcoalclusterrecord
R 3943 5 7 migcoale_typedef_simrecord lastupdateaveseptime migcoalclusterrecord
R 3944 5 8 migcoale_typedef_simrecord rescalecount migcoalclusterrecord
R 3945 5 9 migcoale_typedef_simrecord sweepoutcount migcoalclusterrecord
R 3946 5 10 migcoale_typedef_simrecord hsizestatistic_totalbox migcoalclusterrecord
R 3947 5 11 migcoale_typedef_simrecord hsizestatistic_eachbox migcoalclusterrecord
R 3948 5 12 migcoale_typedef_simrecord lastoutsizedisttime_integralbox migcoalclusterrecord
R 3949 5 13 migcoale_typedef_simrecord lastoutsizedisttime_eachbox migcoalclusterrecord
R 3957 5 21 migcoale_typedef_simrecord initsimulationrecord$tbp$0 migcoalclusterrecord
R 3958 5 22 migcoale_typedef_simrecord setsimusteps$tbp$1 migcoalclusterrecord
R 3959 5 23 migcoale_typedef_simrecord getsimusteps$tbp$2 migcoalclusterrecord
R 3960 5 24 migcoale_typedef_simrecord increaseonesimustep$tbp$3 migcoalclusterrecord
R 3961 5 25 migcoale_typedef_simrecord setsimutimes$tbp$4 migcoalclusterrecord
R 3962 5 26 migcoale_typedef_simrecord getsimutimes$tbp$5 migcoalclusterrecord
R 3963 5 27 migcoale_typedef_simrecord addsimutimes$tbp$6 migcoalclusterrecord
R 3964 5 28 migcoale_typedef_simrecord settimesteps$tbp$7 migcoalclusterrecord
R 3965 5 29 migcoale_typedef_simrecord gettimesteps$tbp$8 migcoalclusterrecord
R 3966 5 30 migcoale_typedef_simrecord setsimupatch$tbp$9 migcoalclusterrecord
R 3967 5 31 migcoale_typedef_simrecord getsimupatch$tbp$10 migcoalclusterrecord
R 3968 5 32 migcoale_typedef_simrecord settimesections$tbp$11 migcoalclusterrecord
R 3969 5 33 migcoale_typedef_simrecord gettimesections$tbp$12 migcoalclusterrecord
R 3970 5 34 migcoale_typedef_simrecord increaseonetimesection$tbp$13 migcoalclusterrecord
R 3971 5 35 migcoale_typedef_simrecord getlastupdatestatistime$tbp$14 migcoalclusterrecord
R 3972 5 36 migcoale_typedef_simrecord setlastupdatestatistime$tbp$15 migcoalclusterrecord
R 3973 5 37 migcoale_typedef_simrecord getlastupdatenltime$tbp$16 migcoalclusterrecord
R 3974 5 38 migcoale_typedef_simrecord setlastupdatenltime$tbp$17 migcoalclusterrecord
R 3975 5 39 migcoale_typedef_simrecord getlastupdatenlnc0$tbp$18 migcoalclusterrecord
R 3976 5 40 migcoale_typedef_simrecord setlastupdatenlnc0$tbp$19 migcoalclusterrecord
R 3977 5 41 migcoale_typedef_simrecord recordnc_forsweepout$tbp$20 migcoalclusterrecord
R 3978 5 42 migcoale_typedef_simrecord getlastrecordoutconfigtime$tbp$21 migcoalclusterrecord
R 3979 5 43 migcoale_typedef_simrecord setlastrecordoutconfigtime$tbp$22 migcoalclusterrecord
R 3980 5 44 migcoale_typedef_simrecord getoutputindex$tbp$23 migcoalclusterrecord
R 3981 5 45 migcoale_typedef_simrecord setoutputindex$tbp$24 migcoalclusterrecord
R 3982 5 46 migcoale_typedef_simrecord increaseoneoutputindex$tbp$25 migcoalclusterrecord
R 3983 5 47 migcoale_typedef_simrecord turnontriggerfocusedtimepoints$tbp$26 migcoalclusterrecord
R 3984 5 48 migcoale_typedef_simrecord turnofftriggerfocusedtimepoints$tbp$27 migcoalclusterrecord
R 3985 5 49 migcoale_typedef_simrecord getstatustriggerfocusedtimepoints$tbp$28 migcoalclusterrecord
R 3986 5 50 migcoale_typedef_simrecord thedefproc$tbp$29 migcoalclusterrecord
R 3987 5 51 migcoale_typedef_simrecord whetheroutsizedist_eachbox$tbp$30 migcoalclusterrecord
R 3988 5 52 migcoale_typedef_simrecord whetheroutsizedist_integralbox$tbp$31 migcoalclusterrecord
R 3989 5 53 migcoale_typedef_simrecord getlastoutsizedisttime_eachbox$tbp$32 migcoalclusterrecord
R 3990 5 54 migcoale_typedef_simrecord setlastoutsizedisttime_eachbox$tbp$33 migcoalclusterrecord
R 3991 5 55 migcoale_typedef_simrecord getlastoutsizedisttime_integralbox$tbp$34 migcoalclusterrecord
R 3992 5 56 migcoale_typedef_simrecord setlastoutsizedisttime_integralbox$tbp$35 migcoalclusterrecord
R 3993 5 57 migcoale_typedef_simrecord getncut$tbp$36 migcoalclusterrecord
R 3994 5 58 migcoale_typedef_simrecord setncut$tbp$37 migcoalclusterrecord
R 3995 5 59 migcoale_typedef_simrecord getlastrecordimplantnum$tbp$38 migcoalclusterrecord
R 3996 5 60 migcoale_typedef_simrecord setlastrecordimplantnum$tbp$39 migcoalclusterrecord
R 3997 5 61 migcoale_typedef_simrecord setimplantedentitiesnum$tbp$40 migcoalclusterrecord
R 3998 5 62 migcoale_typedef_simrecord getimplantedentitiesnum$tbp$41 migcoalclusterrecord
R 3999 5 63 migcoale_typedef_simrecord addimplantedentitiesnum$tbp$42 migcoalclusterrecord
R 4000 5 64 migcoale_typedef_simrecord getsweepoutcount$tbp$43 migcoalclusterrecord
R 4001 5 65 migcoale_typedef_simrecord increaseonesweepoutcount$tbp$44 migcoalclusterrecord
R 4002 5 66 migcoale_typedef_simrecord getrescalecount$tbp$45 migcoalclusterrecord
R 4003 5 67 migcoale_typedef_simrecord increaseonerescalecount$tbp$46 migcoalclusterrecord
R 4004 5 68 migcoale_typedef_simrecord setlastupdateaveseptime$tbp$47 migcoalclusterrecord
R 4005 5 69 migcoale_typedef_simrecord getlastupdateaveseptime$tbp$48 migcoalclusterrecord
R 4006 5 70 migcoale_typedef_simrecord getstartimplanttime$tbp$49 migcoalclusterrecord
R 4007 5 71 migcoale_typedef_simrecord setstartimplanttime$tbp$50 migcoalclusterrecord
R 4008 5 72 migcoale_typedef_simrecord initmigcoalclusterrecord$tbp$51 migcoalclusterrecord
S 4199 16 0 0 0 6 1 624 41651 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 cascadegenway_bycentuniform_locally
S 4200 16 0 0 0 6 1 624 41687 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1 3 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 cascadegenway_bymddatabase_locally
S 4201 16 0 0 0 6 1 624 41722 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 2 18 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 cascadegenway_bymddatabase_uniform
S 4202 25 0 0 0 3737 1 624 41757 10000004 800010 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4204 0 0 0 624 0 0 0 0 clusteratom
S 4203 5 0 0 0 3743 1 624 33319 800004 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 3737 0 0 0 0 0 0 0 0 0 0 0 1 4203 0 624 0 0 0 0 pos
S 4204 8 5 0 0 3746 1 624 41769 40822004 1220 A 0 0 0 0 B 0 0 0 0 0 0 0 3737 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 mc_generatecascadebox$clusteratom$td
S 4205 25 0 0 0 3749 1 624 41806 1000000c 800250 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 4376 0 0 0 624 0 0 0 0 mdstatistic
S 4206 5 0 0 0 6 4207 624 41818 800004 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 1 4206 0 624 0 0 0 0 maxsianumeachcluster
S 4207 5 0 0 0 6 4209 624 41844 800004 0 A 0 0 0 0 B 0 0 0 0 0 4 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4206 4207 0 624 0 0 0 0 minsianumeachcluster
S 4208 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1184086197 -1257935337 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 4209 5 0 0 0 6 4210 624 41871 800004 0 A 0 0 0 0 B 0 0 0 0 0 8 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4207 4209 0 624 0 0 0 0 maxvacnumeachcluster
S 4210 5 0 0 0 6 4211 624 41897 800004 0 A 0 0 0 0 B 0 0 0 0 0 12 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4209 4210 0 624 0 0 0 0 minvacnumeachcluster
S 4211 5 0 0 0 6 4212 624 41924 800004 0 A 0 0 0 0 B 0 0 0 0 0 16 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4210 4211 0 624 0 0 0 0 maxmixnumeachcluster
S 4212 5 0 0 0 6 4213 624 41950 800004 0 A 0 0 0 0 B 0 0 0 0 0 20 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4211 4212 0 624 0 0 0 0 minmixnumeachcluster
S 4213 5 0 0 0 9 4214 624 41977 800004 0 A 0 0 0 0 B 0 0 0 0 0 24 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4212 4213 0 624 0 0 0 0 maxdistancesia_tocent
S 4214 5 0 0 0 9 4215 624 42004 800004 0 A 0 0 0 0 B 0 0 0 0 0 32 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4213 4214 0 624 0 0 0 0 maxdistancevac_tocent
S 4215 5 0 0 0 9 4216 624 42031 800004 0 A 0 0 0 0 B 0 0 0 0 0 40 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4214 4215 0 624 0 0 0 0 maxdistancemix_tocent
S 4216 5 0 0 0 9 4217 624 42058 800004 0 A 0 0 0 0 B 0 0 0 0 0 48 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4215 4216 0 624 0 0 0 0 maxdistancesia_betweencluster
S 4217 5 0 0 0 9 4218 624 42093 800004 0 A 0 0 0 0 B 0 0 0 0 0 56 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4216 4217 0 624 0 0 0 0 maxdistancevac_betweencluster
S 4218 5 0 0 0 9 4219 624 42128 800004 0 A 0 0 0 0 B 0 0 0 0 0 64 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4217 4218 0 624 0 0 0 0 maxdistancemix_betweencluster
S 4219 5 0 0 0 9 4220 624 42163 800004 0 A 0 0 0 0 B 0 0 0 0 0 72 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4218 4219 0 624 0 0 0 0 maxdistancesiatovac_betweencluster
S 4220 5 0 0 0 9 4221 624 42203 800004 0 A 0 0 0 0 B 0 0 0 0 0 80 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4219 4220 0 624 0 0 0 0 mindistancesia_tocent
S 4221 5 0 0 0 9 4222 624 42231 800004 0 A 0 0 0 0 B 0 0 0 0 0 88 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4220 4221 0 624 0 0 0 0 mindistancevac_tocent
S 4222 5 0 0 0 9 4223 624 42259 800004 0 A 0 0 0 0 B 0 0 0 0 0 96 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4221 4222 0 624 0 0 0 0 mindistancemix_tocent
S 4223 5 0 0 0 9 4224 624 42287 800004 0 A 0 0 0 0 B 0 0 0 0 0 104 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4222 4223 0 624 0 0 0 0 mindistancesia_betweencluster
S 4224 5 0 0 0 9 4225 624 42323 800004 0 A 0 0 0 0 B 0 0 0 0 0 112 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4223 4224 0 624 0 0 0 0 mindistancevac_betweencluster
S 4225 5 0 0 0 9 4226 624 42359 800004 0 A 0 0 0 0 B 0 0 0 0 0 120 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4224 4225 0 624 0 0 0 0 mindistancemix_betweencluster
S 4226 5 0 0 0 9 4227 624 42395 800004 0 A 0 0 0 0 B 0 0 0 0 0 128 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4225 4226 0 624 0 0 0 0 mindistancesiatovac_betweencluster
S 4227 5 0 0 0 9 4228 624 42436 800004 0 A 0 0 0 0 B 0 0 0 0 0 136 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4226 4227 0 624 0 0 0 0 binwidthsia_tocenter
S 4228 5 0 0 0 9 4229 624 42462 800004 0 A 0 0 0 0 B 0 0 0 0 0 144 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4227 4228 0 624 0 0 0 0 binwidthvac_tocenter
S 4229 5 0 0 0 9 4230 624 42488 800004 0 A 0 0 0 0 B 0 0 0 0 0 152 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4228 4229 0 624 0 0 0 0 binwidthmix_tocenter
S 4230 5 0 0 0 9 4231 624 42514 800004 0 A 0 0 0 0 B 0 0 0 0 0 160 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4229 4230 0 624 0 0 0 0 binwidthsia_betweencluster
S 4231 5 0 0 0 9 4232 624 42546 800004 0 A 0 0 0 0 B 0 0 0 0 0 168 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4230 4231 0 624 0 0 0 0 binwidthvac_betweencluster
S 4232 5 0 0 0 9 4233 624 42578 800004 0 A 0 0 0 0 B 0 0 0 0 0 176 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4231 4232 0 624 0 0 0 0 binwidthmix_betweencluster
S 4233 5 0 0 0 9 4235 624 42610 800004 0 A 0 0 0 0 B 0 0 0 0 0 184 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4232 4233 0 624 0 0 0 0 binwidthsiatovac_betweencluster
S 4234 6 4 0 0 6 4240 624 37221 40800006 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_0_2
S 4235 5 6 0 0 3755 4237 624 42647 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 192 4237 0 3749 0 4239 0 0 0 0 0 0 0 0 4236 4233 4235 4238 624 0 0 0 0 tocent_distsia
S 4236 5 0 0 0 3758 4241 624 42662 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 208 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4238 4236 0 624 0 0 0 0 tocent_distsia$sd
S 4237 5 0 0 0 7 4238 624 42680 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 192 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4235 4237 0 624 0 0 0 0 tocent_distsia$p
S 4238 5 0 0 0 7 4236 624 42697 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 200 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4237 4238 0 624 0 0 0 0 tocent_distsia$o
S 4239 22 1 0 0 8 1 624 42714 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4235 0 0 0 0 4236 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tocent_distsia$arrdsc
S 4240 6 4 0 0 6 4246 624 42736 40800006 0 A 0 0 0 0 B 0 0 0 0 0 4 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_1_2
S 4241 5 6 0 0 3761 4243 624 42744 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 280 4243 0 3749 0 4245 0 0 0 0 0 0 0 0 4242 4235 4241 4244 624 0 0 0 0 tocent_distvac
S 4242 5 0 0 0 3764 4247 624 42759 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 296 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4244 4242 0 624 0 0 0 0 tocent_distvac$sd
S 4243 5 0 0 0 7 4244 624 42777 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 280 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4241 4243 0 624 0 0 0 0 tocent_distvac$p
S 4244 5 0 0 0 7 4242 624 42794 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 288 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4243 4244 0 624 0 0 0 0 tocent_distvac$o
S 4245 22 1 0 0 8 1 624 42811 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4241 0 0 0 0 4242 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tocent_distvac$arrdsc
S 4246 6 4 0 0 6 4252 624 24655 40800006 0 A 0 0 0 0 B 0 0 0 0 0 8 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_2_1
S 4247 5 6 0 0 3767 4249 624 42833 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 368 4249 0 3749 0 4251 0 0 0 0 0 0 0 0 4248 4241 4247 4250 624 0 0 0 0 tocent_distmix
S 4248 5 0 0 0 3770 4253 624 42848 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 384 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4250 4248 0 624 0 0 0 0 tocent_distmix$sd
S 4249 5 0 0 0 7 4250 624 42866 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 368 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4247 4249 0 624 0 0 0 0 tocent_distmix$p
S 4250 5 0 0 0 7 4248 624 42883 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 376 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4249 4250 0 624 0 0 0 0 tocent_distmix$o
S 4251 22 1 0 0 8 1 624 42900 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4247 0 0 0 0 4248 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tocent_distmix$arrdsc
S 4252 6 4 0 0 6 4258 624 37229 40800006 0 A 0 0 0 0 B 0 0 0 0 0 12 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_3_1
S 4253 5 6 0 0 3773 4255 624 42922 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 456 4255 0 3749 0 4257 0 0 0 0 0 0 0 0 4254 4247 4253 4256 624 0 0 0 0 clustergap_distsia
S 4254 5 0 0 0 3776 4259 624 42941 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 472 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4256 4254 0 624 0 0 0 0 clustergap_distsia$sd
S 4255 5 0 0 0 7 4256 624 42963 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 456 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4253 4255 0 624 0 0 0 0 clustergap_distsia$p
S 4256 5 0 0 0 7 4254 624 42984 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 464 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4255 4256 0 624 0 0 0 0 clustergap_distsia$o
S 4257 22 1 0 0 8 1 624 43005 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4253 0 0 0 0 4254 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 clustergap_distsia$arrdsc
S 4258 6 4 0 0 6 4264 624 43031 40800006 0 A 0 0 0 0 B 0 0 0 0 0 16 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_4_1
S 4259 5 6 0 0 3779 4261 624 43039 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 544 4261 0 3749 0 4263 0 0 0 0 0 0 0 0 4260 4253 4259 4262 624 0 0 0 0 clustergap_distvac
S 4260 5 0 0 0 3782 4265 624 43058 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 560 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4262 4260 0 624 0 0 0 0 clustergap_distvac$sd
S 4261 5 0 0 0 7 4262 624 43080 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 544 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4259 4261 0 624 0 0 0 0 clustergap_distvac$p
S 4262 5 0 0 0 7 4260 624 43101 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 552 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4261 4262 0 624 0 0 0 0 clustergap_distvac$o
S 4263 22 1 0 0 8 1 624 43122 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4259 0 0 0 0 4260 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 clustergap_distvac$arrdsc
S 4264 6 4 0 0 6 4270 624 43148 40800006 0 A 0 0 0 0 B 0 0 0 0 0 20 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_5_1
S 4265 5 6 0 0 3785 4267 624 43156 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 632 4267 0 3749 0 4269 0 0 0 0 0 0 0 0 4266 4259 4265 4268 624 0 0 0 0 clustergap_distmix
S 4266 5 0 0 0 3788 4271 624 43175 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 648 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4268 4266 0 624 0 0 0 0 clustergap_distmix$sd
S 4267 5 0 0 0 7 4268 624 43197 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 632 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4265 4267 0 624 0 0 0 0 clustergap_distmix$p
S 4268 5 0 0 0 7 4266 624 43218 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 640 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4267 4268 0 624 0 0 0 0 clustergap_distmix$o
S 4269 22 1 0 0 8 1 624 43239 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4265 0 0 0 0 4266 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 clustergap_distmix$arrdsc
S 4270 6 4 0 0 6 4276 624 43265 40800006 0 A 0 0 0 0 B 0 0 0 0 0 24 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_6_1
S 4271 5 6 0 0 3791 4273 624 43273 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 720 4273 0 3749 0 4275 0 0 0 0 0 0 0 0 4272 4265 4271 4274 624 0 0 0 0 clustergap_siatovac
S 4272 5 0 0 0 3794 4277 624 43293 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 736 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4274 4272 0 624 0 0 0 0 clustergap_siatovac$sd
S 4273 5 0 0 0 7 4274 624 43316 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 720 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4271 4273 0 624 0 0 0 0 clustergap_siatovac$p
S 4274 5 0 0 0 7 4272 624 43338 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 728 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4273 4274 0 624 0 0 0 0 clustergap_siatovac$o
S 4275 22 1 0 0 8 1 624 43360 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4271 0 0 0 0 4272 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 clustergap_siatovac$arrdsc
S 4276 6 4 0 0 6 4282 624 43387 40800006 0 A 0 0 0 0 B 0 0 0 0 0 28 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_7_1
S 4277 5 6 0 0 3797 4279 624 43395 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 808 4279 0 3749 0 4281 0 0 0 0 0 0 0 0 4278 4271 4277 4280 624 0 0 0 0 natomsia
S 4278 5 0 0 0 3800 4283 624 43404 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 824 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4280 4278 0 624 0 0 0 0 natomsia$sd
S 4279 5 0 0 0 7 4280 624 43416 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 808 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4277 4279 0 624 0 0 0 0 natomsia$p
S 4280 5 0 0 0 7 4278 624 43427 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 816 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4279 4280 0 624 0 0 0 0 natomsia$o
S 4281 22 1 0 0 6 1 624 43438 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4277 0 0 0 0 4278 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 natomsia$arrdsc
S 4282 6 4 0 0 6 4288 624 43454 40800006 0 A 0 0 0 0 B 0 0 0 0 0 32 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_8_1
S 4283 5 6 0 0 3803 4285 624 43462 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 896 4285 0 3749 0 4287 0 0 0 0 0 0 0 0 4284 4277 4283 4286 624 0 0 0 0 natomvac
S 4284 5 0 0 0 3806 4289 624 43471 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 912 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4286 4284 0 624 0 0 0 0 natomvac$sd
S 4285 5 0 0 0 7 4286 624 43483 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 896 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4283 4285 0 624 0 0 0 0 natomvac$p
S 4286 5 0 0 0 7 4284 624 43494 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 904 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4285 4286 0 624 0 0 0 0 natomvac$o
S 4287 22 1 0 0 6 1 624 43505 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4283 0 0 0 0 4284 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 natomvac$arrdsc
S 4288 6 4 0 0 6 4294 624 43521 40800006 0 A 0 0 0 0 B 0 0 0 0 0 36 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_9_1
S 4289 5 6 0 0 3809 4291 624 43529 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 984 4291 0 3749 0 4293 0 0 0 0 0 0 0 0 4290 4283 4289 4292 624 0 0 0 0 natommix
S 4290 5 0 0 0 3812 4295 624 43538 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 1000 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4292 4290 0 624 0 0 0 0 natommix$sd
S 4291 5 0 0 0 7 4292 624 43550 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 984 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4289 4291 0 624 0 0 0 0 natommix$p
S 4292 5 0 0 0 7 4290 624 43561 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 992 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4291 4292 0 624 0 0 0 0 natommix$o
S 4293 22 1 0 0 6 1 624 43572 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4289 0 0 0 0 4290 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 natommix$arrdsc
S 4294 6 4 0 0 6 4300 624 43588 40800006 0 A 0 0 0 0 B 0 0 0 0 0 40 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_10_1
S 4295 5 6 0 0 3815 4297 624 43597 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 1072 4297 0 3749 0 4299 0 0 0 0 0 0 0 0 4296 4289 4295 4298 624 0 0 0 0 binsia_tocenterarray
S 4296 5 0 0 0 3818 4301 624 43618 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 1088 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4298 4296 0 624 0 0 0 0 binsia_tocenterarray$sd
S 4297 5 0 0 0 7 4298 624 43642 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 1072 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4295 4297 0 624 0 0 0 0 binsia_tocenterarray$p
S 4298 5 0 0 0 7 4296 624 43665 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 1080 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4297 4298 0 624 0 0 0 0 binsia_tocenterarray$o
S 4299 22 1 0 0 8 1 624 43688 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4295 0 0 0 0 4296 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 binsia_tocenterarray$arrdsc
S 4300 6 4 0 0 6 4306 624 43716 40800006 0 A 0 0 0 0 B 0 0 0 0 0 44 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_11_1
S 4301 5 6 0 0 3821 4303 624 43725 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 1160 4303 0 3749 0 4305 0 0 0 0 0 0 0 0 4302 4295 4301 4304 624 0 0 0 0 binsia_betweenclusterarray
S 4302 5 0 0 0 3824 4307 624 43752 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 1176 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4304 4302 0 624 0 0 0 0 binsia_betweenclusterarray$sd
S 4303 5 0 0 0 7 4304 624 43782 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 1160 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4301 4303 0 624 0 0 0 0 binsia_betweenclusterarray$p
S 4304 5 0 0 0 7 4302 624 43811 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 1168 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4303 4304 0 624 0 0 0 0 binsia_betweenclusterarray$o
S 4305 22 1 0 0 8 1 624 43840 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4301 0 0 0 0 4302 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 binsia_betweenclusterarray$arrdsc
S 4306 6 4 0 0 6 4312 624 43874 40800006 0 A 0 0 0 0 B 0 0 0 0 0 48 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_12_1
S 4307 5 6 0 0 3827 4309 624 43883 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 1248 4309 0 3749 0 4311 0 0 0 0 0 0 0 0 4308 4301 4307 4310 624 0 0 0 0 binvac_tocenterarray
S 4308 5 0 0 0 3830 4313 624 43904 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 1264 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4310 4308 0 624 0 0 0 0 binvac_tocenterarray$sd
S 4309 5 0 0 0 7 4310 624 43928 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 1248 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4307 4309 0 624 0 0 0 0 binvac_tocenterarray$p
S 4310 5 0 0 0 7 4308 624 43951 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 1256 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4309 4310 0 624 0 0 0 0 binvac_tocenterarray$o
S 4311 22 1 0 0 8 1 624 43974 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4307 0 0 0 0 4308 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 binvac_tocenterarray$arrdsc
S 4312 6 4 0 0 6 4318 624 44002 40800006 0 A 0 0 0 0 B 0 0 0 0 0 52 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_13
S 4313 5 6 0 0 3833 4315 624 44009 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 1336 4315 0 3749 0 4317 0 0 0 0 0 0 0 0 4314 4307 4313 4316 624 0 0 0 0 binvac_betweenclusterarray
S 4314 5 0 0 0 3836 4319 624 44036 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 1352 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4316 4314 0 624 0 0 0 0 binvac_betweenclusterarray$sd
S 4315 5 0 0 0 7 4316 624 44066 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 1336 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4313 4315 0 624 0 0 0 0 binvac_betweenclusterarray$p
S 4316 5 0 0 0 7 4314 624 44095 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 1344 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4315 4316 0 624 0 0 0 0 binvac_betweenclusterarray$o
S 4317 22 1 0 0 8 1 624 44124 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4313 0 0 0 0 4314 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 binvac_betweenclusterarray$arrdsc
S 4318 6 4 0 0 6 4324 624 44158 40800006 0 A 0 0 0 0 B 0 0 0 0 0 56 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_14
S 4319 5 6 0 0 3839 4321 624 44165 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 1424 4321 0 3749 0 4323 0 0 0 0 0 0 0 0 4320 4313 4319 4322 624 0 0 0 0 binsiatovac_betweenclusterarray
S 4320 5 0 0 0 3842 4325 624 44197 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 1440 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4322 4320 0 624 0 0 0 0 binsiatovac_betweenclusterarray$sd
S 4321 5 0 0 0 7 4322 624 44232 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 1424 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4319 4321 0 624 0 0 0 0 binsiatovac_betweenclusterarray$p
S 4322 5 0 0 0 7 4320 624 44266 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 1432 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4321 4322 0 624 0 0 0 0 binsiatovac_betweenclusterarray$o
S 4323 22 1 0 0 8 1 624 44300 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4319 0 0 0 0 4320 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 binsiatovac_betweenclusterarray$arrdsc
S 4324 6 4 0 0 6 4330 624 44339 40800006 0 A 0 0 0 0 B 0 0 0 0 0 60 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_15
S 4325 5 6 0 0 3845 4327 624 44346 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 1512 4327 0 3749 0 4329 0 0 0 0 0 0 0 0 4326 4319 4325 4328 624 0 0 0 0 binmix_tocenterarray
S 4326 5 0 0 0 3848 4331 624 44367 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 1528 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4328 4326 0 624 0 0 0 0 binmix_tocenterarray$sd
S 4327 5 0 0 0 7 4328 624 44391 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 1512 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4325 4327 0 624 0 0 0 0 binmix_tocenterarray$p
S 4328 5 0 0 0 7 4326 624 44414 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 1520 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4327 4328 0 624 0 0 0 0 binmix_tocenterarray$o
S 4329 22 1 0 0 8 1 624 44437 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4325 0 0 0 0 4326 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 binmix_tocenterarray$arrdsc
S 4330 6 4 0 0 6 4336 624 44465 40800006 0 A 0 0 0 0 B 0 0 0 0 0 64 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_16
S 4331 5 6 0 0 3851 4333 624 44472 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 1600 4333 0 3749 0 4335 0 0 0 0 0 0 0 0 4332 4325 4331 4334 624 0 0 0 0 binmix_betweenclusterarray
S 4332 5 0 0 0 3854 4337 624 44499 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 1616 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4334 4332 0 624 0 0 0 0 binmix_betweenclusterarray$sd
S 4333 5 0 0 0 7 4334 624 44529 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 1600 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4331 4333 0 624 0 0 0 0 binmix_betweenclusterarray$p
S 4334 5 0 0 0 7 4332 624 44558 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 1608 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4333 4334 0 624 0 0 0 0 binmix_betweenclusterarray$o
S 4335 22 1 0 0 8 1 624 44587 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4331 0 0 0 0 4332 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 binmix_betweenclusterarray$arrdsc
S 4336 6 4 0 0 6 4342 624 44621 40800006 0 A 0 0 0 0 B 0 0 0 0 0 68 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_17
S 4337 5 6 0 0 3857 4339 624 44628 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 1688 4339 0 3749 0 4341 0 0 0 0 0 0 0 0 4338 4331 4337 4340 624 0 0 0 0 binsia_natomarray
S 4338 5 0 0 0 3860 4343 624 44646 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 1704 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4340 4338 0 624 0 0 0 0 binsia_natomarray$sd
S 4339 5 0 0 0 7 4340 624 44667 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 1688 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4337 4339 0 624 0 0 0 0 binsia_natomarray$p
S 4340 5 0 0 0 7 4338 624 44687 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 1696 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4339 4340 0 624 0 0 0 0 binsia_natomarray$o
S 4341 22 1 0 0 8 1 624 44707 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4337 0 0 0 0 4338 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 binsia_natomarray$arrdsc
S 4342 6 4 0 0 6 4348 624 44732 40800006 0 A 0 0 0 0 B 0 0 0 0 0 72 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_18
S 4343 5 6 0 0 3863 4345 624 44739 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 1776 4345 0 3749 0 4347 0 0 0 0 0 0 0 0 4344 4337 4343 4346 624 0 0 0 0 binvac_natomarray
S 4344 5 0 0 0 3866 4349 624 44757 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 1792 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4346 4344 0 624 0 0 0 0 binvac_natomarray$sd
S 4345 5 0 0 0 7 4346 624 44778 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 1776 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4343 4345 0 624 0 0 0 0 binvac_natomarray$p
S 4346 5 0 0 0 7 4344 624 44798 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 1784 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4345 4346 0 624 0 0 0 0 binvac_natomarray$o
S 4347 22 1 0 0 8 1 624 44818 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4343 0 0 0 0 4344 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 binvac_natomarray$arrdsc
S 4348 6 4 0 0 6 1 624 44843 40800006 0 A 0 0 0 0 B 0 0 0 0 0 76 0 0 0 0 0 0 4389 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z_b_19
S 4349 5 6 0 0 3869 4351 624 44850 10a00004 51 A 0 0 0 0 B 0 0 0 0 0 1864 4351 0 3749 0 4353 0 0 0 0 0 0 0 0 4350 4343 4349 4352 624 0 0 0 0 binmix_natomarray
S 4350 5 0 0 0 3872 4354 624 44868 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 1880 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4352 4350 0 624 0 0 0 0 binmix_natomarray$sd
S 4351 5 0 0 0 7 4352 624 44889 40802001 1020 A 0 0 0 0 B 0 0 0 0 0 1864 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4349 4351 0 624 0 0 0 0 binmix_natomarray$p
S 4352 5 0 0 0 7 4350 624 44909 40802000 1020 A 0 0 0 0 B 0 0 0 0 0 1872 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4351 4352 0 624 0 0 0 0 binmix_natomarray$o
S 4353 22 1 0 0 8 1 624 44929 40000000 1000 A 0 0 0 0 B 0 0 0 0 0 0 0 4349 0 0 0 0 4350 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 binmix_natomarray$arrdsc
S 4354 5 0 0 0 6 4355 624 44954 800004 0 A 0 0 0 0 B 0 0 0 0 0 1952 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4349 4354 0 624 0 0 0 0 nfrankelpariseachbox_ave
S 4355 5 0 0 0 9 4356 624 44979 800004 0 A 0 0 0 0 B 0 0 0 0 0 1960 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4354 4355 0 624 0 0 0 0 totalcountsia_gapctoc
S 4356 5 0 0 0 9 4357 624 45001 800004 0 A 0 0 0 0 B 0 0 0 0 0 1968 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4355 4356 0 624 0 0 0 0 totalcountvac_gapctoc
S 4357 5 0 0 0 9 4358 624 45023 800004 0 A 0 0 0 0 B 0 0 0 0 0 1976 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4356 4357 0 624 0 0 0 0 totalcount_gapsiatovac
S 4358 5 0 0 0 9 4359 624 45046 800004 0 A 0 0 0 0 B 0 0 0 0 0 1984 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4357 4358 0 624 0 0 0 0 totalcountmix_gapctoc
S 4359 5 0 0 0 9 4360 624 45068 800004 0 A 0 0 0 0 B 0 0 0 0 0 1992 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4358 4359 0 624 0 0 0 0 totalcountsia_tocent
S 4360 5 0 0 0 9 4361 624 45089 800004 0 A 0 0 0 0 B 0 0 0 0 0 2000 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4359 4360 0 624 0 0 0 0 totalcountvac_tocent
S 4361 5 0 0 0 9 4362 624 45110 800004 0 A 0 0 0 0 B 0 0 0 0 0 2008 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4360 4361 0 624 0 0 0 0 totalcountmix_tocent
S 4362 5 0 0 0 9 4363 624 45131 800004 0 A 0 0 0 0 B 0 0 0 0 0 2016 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4361 4362 0 624 0 0 0 0 totalcountsia_natom
S 4363 5 0 0 0 9 4364 624 45151 800004 0 A 0 0 0 0 B 0 0 0 0 0 2024 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4362 4363 0 624 0 0 0 0 totalcountvac_natom
S 4364 5 0 0 0 9 4372 624 45171 800004 0 A 0 0 0 0 B 0 0 0 0 0 2032 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 4363 4364 0 624 0 0 0 0 totalcountmix_natom
S 4367 14 0 0 0 8 1 624 45216 80 402200 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3749 1 0 0 0 0 0 0 0 copymdstatisticfromother$tbp
S 4370 14 0 0 0 8 1 624 45263 0 2200 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 3749 0 0 0 0 0 0 0 0 clean_mdstatistic$tbp
S 4371 27 0 0 0 8 4421 624 45285 10 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 cleanmdstatistic
S 4372 5 0 0 0 6 4373 624 45302 800002 2200 A 0 0 1 0 B 0 0 0 0 0 0 0 0 3749 0 0 0 0 0 0 0 0 0 4423 0 0 0 0 0 0 0 0 0 cleanmdstatistic$0
S 4373 5 0 0 0 8 4374 624 45321 800002 2200 A 0 0 0 0 B 0 0 0 0 0 0 0 0 3749 0 0 0 0 0 0 3918 0 0 4367 0 0 0 0 0 0 0 0 0 =$12
S 4374 5 0 0 0 6 4375 624 45326 800002 102200 A 0 0 0 0 B 0 0 0 0 0 0 0 0 3749 0 0 0 0 0 0 4370 0 0 4420 0 0 0 0 0 0 0 0 0 clean_mdstatistic$tbp$13
S 4375 5 0 0 0 6 1 624 45351 800002 102200 A 0 0 0 0 B 0 0 0 0 0 0 0 0 3749 0 0 0 0 0 0 4367 0 0 4417 0 0 0 0 0 0 0 0 0 copymdstatisticfromother$tbp$14
S 4376 8 5 0 0 3915 1 624 45383 40822004 1220 A 0 0 0 0 B 0 0 0 0 0 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 mc_generatecascadebox$mdstatistic$td
S 4377 19 0 0 0 8 1 624 45420 4000 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 270 2 0 0 0 0 0 624 0 0 0 0 copyarray
O 4377 2 4379 4378
S 4378 27 0 0 0 8 4390 624 45430 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 296 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 copyarray_d
Q 4378 4377 0
S 4379 27 0 0 0 8 4402 624 45442 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 297 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 copyarray_i
Q 4379 4377 0
S 4380 16 0 0 0 6 1 624 45454 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 4 15 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 maxnumlen
S 4381 16 0 0 0 6 1 624 45464 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 100 53 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 binnum
S 4382 16 0 0 0 6 1 624 45471 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1 3 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 siaindex_md
S 4383 16 0 0 0 6 1 624 45483 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 2 18 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 siaindex2_md
S 4384 16 0 0 0 6 1 624 45496 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 3 49 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 siaindex3_md
S 4385 16 0 0 0 6 1 624 45509 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 4 15 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 vacindex_md
S 4386 16 0 0 0 6 1 624 45521 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 8 13 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 refsiteindex_md
S 4387 16 0 0 0 9 1 624 45537 4 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 662 84 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 p_roundoff
S 4388 26 0 0 0 0 1 624 8612 4 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 295 25 0 0 0 0 0 624 0 0 0 0 =
O 4388 25 3115 2862 3409 3425 3460 2999 2973 2962 2945 2751 2727 2719 2695 2013 2030 2076 1956 1888 3284 1399 1420 1511 1528 3630 1125
S 4389 11 0 0 0 8 4193 624 45554 40800000 805000 A 0 0 0 0 B 0 0 0 0 0 80 0 0 4234 4348 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 _mc_generatecascadebox$0
S 4390 23 5 0 0 0 4393 624 45430 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 copyarray_d
S 4391 7 3 2 0 3918 1 4390 8850 10a00004 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4394 0 0 0 0 0 0 0 0 this
S 4392 7 3 1 0 3924 1 4390 10541 10a00004 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4398 0 0 0 0 0 0 0 0 others
S 4393 14 5 0 0 0 1 4390 45430 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1332 2 0 0 0 0 0 0 0 0 0 0 0 0 112 0 624 0 0 0 0 copyarray_d
F 4393 2 4391 4392
S 4394 8 1 0 0 3921 1 4390 45579 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 this$sd1
S 4398 8 1 0 0 3927 1 4390 45614 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 others$sd
S 4402 23 5 0 0 0 4405 624 45442 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 copyarray_i
S 4403 7 3 2 0 3930 1 4402 8850 10a00004 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4406 0 0 0 0 0 0 0 0 this
S 4404 7 3 1 0 3936 1 4402 10541 10a00004 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4410 0 0 0 0 0 0 0 0 others
S 4405 14 5 0 0 0 1 4402 45442 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1335 2 0 0 0 0 0 0 0 0 0 0 0 0 131 0 624 0 0 0 0 copyarray_i
F 4405 2 4403 4404
S 4406 8 1 0 0 3933 1 4402 45656 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 this$sd2
S 4410 8 1 0 0 3939 1 4402 45694 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 others$sd6
S 4414 23 5 0 0 0 4417 624 45191 10 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 copymdstatisticfromother
S 4415 1 3 2 0 3749 1 4414 8850 4 3200 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 this
S 4416 1 3 1 0 3749 1 4414 10541 4 3200 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 others
S 4417 14 5 0 0 0 1 4414 45191 90 400200 A 0 0 0 0 B 0 0 0 0 0 0 0 1338 2 0 0 0 0 0 0 0 0 0 0 0 0 150 0 624 0 0 0 0 copymdstatisticfromother
F 4417 2 4415 4416
S 4418 23 5 0 0 0 4420 624 45245 10 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 clean_mdstatistic
S 4419 1 3 0 0 3749 1 4418 8850 4 3200 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 this
S 4420 14 5 0 0 0 1 4418 45245 90 400200 A 0 0 0 0 B 0 0 0 0 0 0 0 1341 1 0 0 0 0 0 0 0 0 0 0 0 0 243 0 624 0 0 0 0 clean_mdstatistic
F 4420 1 4419
S 4421 23 5 0 0 0 4423 624 45285 10 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 cleanmdstatistic
S 4422 1 3 0 0 3749 1 4421 8850 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 this
S 4423 14 5 0 0 0 1 4421 45285 10 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1343 1 0 0 0 0 0 0 0 0 0 0 0 0 339 0 624 0 0 0 0 cleanmdstatistic
F 4423 1 4422
S 4424 23 5 0 0 0 4431 624 45740 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 cascadedatabase_analysis_siaandvac
S 4425 1 3 1 0 3942 1 4424 45775 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 pathin
S 4426 1 3 1 0 6 1 4424 45782 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 index_startbox
S 4427 1 3 1 0 6 1 4424 45797 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 index_endbox
S 4428 1 3 1 0 6 1 4424 45810 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 index_siaconfig
S 4429 1 3 1 0 6 1 4424 45826 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 index_vacconfig
S 4430 1 3 0 0 3749 1 4424 45842 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 themdstatistic
S 4431 14 5 0 0 0 1 4424 45740 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1345 6 0 0 0 0 0 0 0 0 0 0 0 0 351 0 624 0 0 0 0 cascadedatabase_analysis_siaandvac
F 4431 6 4425 4426 4427 4428 4429 4430
S 4432 23 5 0 0 0 4437 624 45857 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 readonconifg_sia
S 4433 1 3 1 0 3944 1 4432 9016 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 filename
S 4434 7 3 0 0 3946 1 4432 45874 10a01004 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4438 0 0 0 0 0 0 0 0 clustersarray
S 4435 7 3 0 0 3952 1 4432 45888 10a00004 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4442 0 0 0 0 0 0 0 0 natomeachcluster
S 4436 1 3 0 0 6 1 4432 45905 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nclustereachbox
S 4437 14 5 0 0 0 1 4432 45857 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1352 4 0 0 0 0 0 0 0 0 0 0 0 0 1142 0 624 0 0 0 0 readonconifg_sia
F 4437 4 4433 4434 4435 4436
S 4438 8 1 0 0 3949 1 4432 45921 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 clustersarray$sd
S 4442 8 1 0 0 3955 1 4432 45991 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 natomeachcluster$sd
S 4446 23 5 0 0 0 4451 624 46073 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 readonconifg_vac
S 4447 1 3 1 0 3958 1 4446 9016 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 filename
S 4448 7 3 0 0 3960 1 4446 45874 10a01004 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4452 0 0 0 0 0 0 0 0 clustersarray
S 4449 7 3 0 0 3966 1 4446 45888 10a00004 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4456 0 0 0 0 0 0 0 0 natomeachcluster
S 4450 1 3 0 0 6 1 4446 45905 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nclustereachbox
S 4451 14 5 0 0 0 1 4446 46073 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1357 4 0 0 0 0 0 0 0 0 0 0 0 0 1353 0 624 0 0 0 0 readonconifg_vac
F 4451 4 4447 4448 4449 4450
S 4452 8 1 0 0 3963 1 4446 46090 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 clustersarray$sd10
S 4456 8 1 0 0 3969 1 4446 46168 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 natomeachcluster$sd14
S 4460 23 5 0 0 0 4467 624 46258 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 reslovecascadecontrolfile_formmddatabase
S 4461 1 3 1 0 6 1 4460 7712 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 hfile
S 4462 1 3 2 0 16 1 4460 46299 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 whetherincludesia
S 4463 1 3 2 0 16 1 4460 46317 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 whetherincludevac
S 4464 1 3 2 0 6 1 4460 46335 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 cascadenum
S 4465 1 3 2 0 16 1 4460 46346 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 whethercascadesameinonebox
S 4466 1 3 0 0 3749 1 4460 45842 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 themdstatistic
S 4467 14 5 0 0 0 1 4460 46258 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1362 6 0 0 0 0 0 0 0 0 0 0 0 0 1525 0 624 0 0 0 0 reslovecascadecontrolfile_formmddatabase
F 4467 6 4461 4462 4463 4464 4465 4466
S 4468 23 5 0 0 0 4474 624 46373 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 generateclusterssize
S 4469 1 3 0 0 3749 1 4468 45842 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 themdstatistic
S 4470 1 3 0 0 6 1 4468 46394 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nsiacluster
S 4471 7 3 0 0 3972 1 4468 46406 10a00004 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4475 0 0 0 0 0 0 0 0 natomeachsiacluster
S 4472 1 3 0 0 6 1 4468 46426 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nvaccluster
S 4473 7 3 0 0 3978 1 4468 46438 10a00004 3050 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4479 0 0 0 0 0 0 0 0 natomeachvaccluster
S 4474 14 5 0 0 0 1 4468 46373 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1369 5 0 0 0 0 0 0 0 0 0 0 0 0 1932 0 624 0 0 0 0 generateclusterssize
F 4474 5 4469 4470 4471 4472 4473
S 4475 8 1 0 0 3975 1 4468 46458 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 natomeachsiacluster$sd
S 4479 8 1 0 0 3981 1 4468 46552 40822004 1020 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 natomeachvaccluster$sd
S 4483 23 5 0 0 0 4485 624 46646 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 generate_cascade_locally_formmddatabase
S 4484 1 3 1 0 6 1 4483 7712 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 hfile
S 4485 14 5 0 0 0 1 4483 46646 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1375 1 0 0 0 0 0 0 0 0 0 0 0 0 2087 0 624 0 0 0 0 generate_cascade_locally_formmddatabase
F 4485 1 4484
S 4486 23 5 0 0 0 4488 624 46686 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 generate_cascade_uniform_formmddatabase
S 4487 1 3 1 0 6 1 4486 7712 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 hfile
S 4488 14 5 0 0 0 1 4486 46686 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1377 1 0 0 0 0 0 0 0 0 0 0 0 0 2523 0 624 0 0 0 0 generate_cascade_uniform_formmddatabase
F 4488 1 4487
S 4489 23 5 0 0 0 4496 624 46726 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 reslovecascadecontrolfile_locally_centuniform
S 4490 1 3 1 0 6 1 4489 7712 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 hfile
S 4491 1 3 2 0 16 1 4489 46299 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 whetherincludesia
S 4492 1 3 2 0 16 1 4489 46317 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 whetherincludevac
S 4493 1 3 2 0 6 1 4489 46772 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 clusternumonecase
S 4494 1 3 2 0 6 1 4489 46335 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 cascadenum
S 4495 1 3 2 0 16 1 4489 46346 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 whethercascadesameinonebox
S 4496 14 5 0 0 0 1 4489 46726 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1379 6 0 0 0 0 0 0 0 0 0 0 0 0 2806 0 624 0 0 0 0 reslovecascadecontrolfile_locally_centuniform
F 4496 6 4490 4491 4492 4493 4494 4495
S 4497 23 5 0 0 0 4499 624 46790 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 generate_cascade_locally_centuniform
S 4498 1 3 1 0 6 1 4497 7712 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 hfile
S 4499 14 5 0 0 0 1 4497 46790 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1386 1 0 0 0 0 0 0 0 0 0 0 0 0 3059 0 624 0 0 0 0 generate_cascade_locally_centuniform
F 4499 1 4498
S 4500 6 0 0 0 3749 1 624 9887 2 10 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4501 0 0 0 0 0 0 0 0 .d0000
S 4501 8 5 0 0 3984 1 624 46827 40022004 1220 A 0 0 1 0 B 0 0 0 0 0 0 0 3749 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 mc_generatecascadebox$mdstatistic$td$ft
A 13 2 0 0 0 6 633 0 0 0 13 0 0 0 0 0 0 0 0 0 0
A 15 2 0 0 0 6 634 0 0 0 15 0 0 0 0 0 0 0 0 0 0
A 18 2 0 0 0 6 635 0 0 0 18 0 0 0 0 0 0 0 0 0 0
A 20 2 0 0 0 9 617 0 0 0 20 0 0 0 0 0 0 0 0 0 0
A 49 2 0 0 0 6 645 0 0 0 49 0 0 0 0 0 0 0 0 0 0
A 51 2 0 0 0 6 646 0 0 0 51 0 0 0 0 0 0 0 0 0 0
A 53 2 0 0 0 6 647 0 0 0 53 0 0 0 0 0 0 0 0 0 0
A 84 2 0 0 0 9 662 0 0 0 84 0 0 0 0 0 0 0 0 0 0
A 141 2 0 0 0 6 771 0 0 0 141 0 0 0 0 0 0 0 0 0 0
A 200 2 0 0 0 6 769 0 0 0 200 0 0 0 0 0 0 0 0 0 0
A 207 2 0 0 0 6 770 0 0 0 207 0 0 0 0 0 0 0 0 0 0
A 210 2 0 0 185 64 773 0 0 0 210 0 0 0 0 0 0 0 0 0 0
A 211 2 0 0 186 64 774 0 0 0 211 0 0 0 0 0 0 0 0 0 0
A 212 2 0 0 187 64 775 0 0 0 212 0 0 0 0 0 0 0 0 0 0
A 213 2 0 0 188 64 776 0 0 0 213 0 0 0 0 0 0 0 0 0 0
A 214 2 0 0 189 64 777 0 0 0 214 0 0 0 0 0 0 0 0 0 0
A 215 2 0 0 191 64 778 0 0 0 215 0 0 0 0 0 0 0 0 0 0
A 216 2 0 0 192 64 779 0 0 0 216 0 0 0 0 0 0 0 0 0 0
A 282 1 0 1 135 66 853 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 307 2 0 0 0 6 888 0 0 0 307 0 0 0 0 0 0 0 0 0 0
A 311 2 0 0 69 6 1111 0 0 0 311 0 0 0 0 0 0 0 0 0 0
A 314 2 0 0 71 6 1112 0 0 0 314 0 0 0 0 0 0 0 0 0 0
A 316 2 0 0 73 6 1113 0 0 0 316 0 0 0 0 0 0 0 0 0 0
A 320 2 0 0 75 6 1114 0 0 0 320 0 0 0 0 0 0 0 0 0 0
A 356 2 0 0 232 275 1391 0 0 0 356 0 0 0 0 0 0 0 0 0 0
A 969 2 0 0 311 16 2005 0 0 0 969 0 0 0 0 0 0 0 0 0 0
A 971 2 0 0 922 1047 1391 0 0 0 971 0 0 0 0 0 0 0 0 0 0
A 1057 1 0 0 252 1184 2643 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1060 1 0 0 651 1193 2645 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1106 2 0 0 1043 1300 1391 0 0 0 1106 0 0 0 0 0 0 0 0 0 0
A 1107 2 0 0 636 6 2676 0 0 0 1107 0 0 0 0 0 0 0 0 0 0
A 1125 2 0 0 688 1457 1391 0 0 0 1125 0 0 0 0 0 0 0 0 0 0
A 1171 2 0 0 0 1612 1391 0 0 0 1171 0 0 0 0 0 0 0 0 0 0
A 1172 2 0 0 0 9 2931 0 0 0 1172 0 0 0 0 0 0 0 0 0 0
A 1173 2 0 0 709 6 2932 0 0 0 1173 0 0 0 0 0 0 0 0 0 0
A 1183 2 0 0 1144 1763 1391 0 0 0 1183 0 0 0 0 0 0 0 0 0 0
A 1208 2 0 0 1110 1875 1391 0 0 0 1208 0 0 0 0 0 0 0 0 0 0
A 1209 2 0 0 124 6 3196 0 0 0 1209 0 0 0 0 0 0 0 0 0 0
A 1210 2 0 0 126 6 3197 0 0 0 1210 0 0 0 0 0 0 0 0 0 0
A 1211 2 0 0 128 6 3198 0 0 0 1211 0 0 0 0 0 0 0 0 0 0
A 1212 2 0 0 130 9 3199 0 0 0 1212 0 0 0 0 0 0 0 0 0 0
A 1213 2 0 0 718 9 3200 0 0 0 1213 0 0 0 0 0 0 0 0 0 0
A 1214 2 0 0 282 6 3201 0 0 0 1214 0 0 0 0 0 0 0 0 0 0
A 1215 2 0 0 1201 8 3202 0 0 0 1215 0 0 0 0 0 0 0 0 0 0
A 1216 2 0 0 287 16 3203 0 0 0 1216 0 0 0 0 0 0 0 0 0 0
A 1217 2 0 0 1203 6 3204 0 0 0 1217 0 0 0 0 0 0 0 0 0 0
A 1246 2 0 0 503 2027 1391 0 0 0 1246 0 0 0 0 0 0 0 0 0 0
A 1362 2 0 0 357 2394 1391 0 0 0 1362 0 0 0 0 0 0 0 0 0 0
A 1363 2 0 0 0 9 3564 0 0 0 1363 0 0 0 0 0 0 0 0 0 0
A 1396 2 0 0 1215 9 4208 0 0 0 1396 0 0 0 0 0 0 0 0 0 0
A 1398 1 0 3 182 3758 4236 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1399 10 0 0 1312 6 1398 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1400 10 0 0 1399 6 1398 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1401 4 0 0 714 6 1400 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1402 4 0 0 1143 6 1399 0 1401 0 0 0 0 1 0 0 0 0 0 0 0
A 1403 10 0 0 1400 6 1398 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1404 10 0 0 1403 6 1398 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1405 10 0 0 1404 6 1398 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1407 1 0 3 0 3764 4242 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1408 10 0 0 1102 6 1407 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1409 10 0 0 1408 6 1407 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1410 4 0 0 1381 6 1409 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1411 4 0 0 991 6 1408 0 1410 0 0 0 0 1 0 0 0 0 0 0 0
A 1412 10 0 0 1409 6 1407 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1413 10 0 0 1412 6 1407 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1414 10 0 0 1413 6 1407 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1416 1 0 3 588 3770 4248 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1417 10 0 0 0 6 1416 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1418 10 0 0 1417 6 1416 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1419 4 0 0 1266 6 1418 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1420 4 0 0 641 6 1417 0 1419 0 0 0 0 1 0 0 0 0 0 0 0
A 1421 10 0 0 1418 6 1416 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1422 10 0 0 1421 6 1416 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1423 10 0 0 1422 6 1416 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1425 1 0 3 1364 3776 4254 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1426 10 0 0 934 6 1425 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1427 10 0 0 1426 6 1425 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1428 4 0 0 1033 6 1427 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1429 4 0 0 1018 6 1426 0 1428 0 0 0 0 1 0 0 0 0 0 0 0
A 1430 10 0 0 1427 6 1425 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1431 10 0 0 1430 6 1425 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1432 10 0 0 1431 6 1425 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1434 1 0 3 1037 3782 4260 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1435 10 0 0 863 6 1434 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1436 10 0 0 1435 6 1434 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1437 4 0 0 1167 6 1436 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1438 4 0 0 847 6 1435 0 1437 0 0 0 0 1 0 0 0 0 0 0 0
A 1439 10 0 0 1436 6 1434 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1440 10 0 0 1439 6 1434 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1441 10 0 0 1440 6 1434 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1443 1 0 3 1166 3788 4266 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1444 10 0 0 706 6 1443 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1445 10 0 0 1444 6 1443 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1446 4 0 0 1172 6 1445 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1447 4 0 0 627 6 1444 0 1446 0 0 0 0 1 0 0 0 0 0 0 0
A 1448 10 0 0 1445 6 1443 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1449 10 0 0 1448 6 1443 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1450 10 0 0 1449 6 1443 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1452 1 0 3 1423 3794 4272 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1453 10 0 0 0 6 1452 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1454 10 0 0 1453 6 1452 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1455 4 0 0 1372 6 1454 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1456 4 0 0 974 6 1453 0 1455 0 0 0 0 1 0 0 0 0 0 0 0
A 1457 10 0 0 1454 6 1452 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1458 10 0 0 1457 6 1452 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1459 10 0 0 1458 6 1452 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1461 1 0 3 0 3800 4278 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1462 10 0 0 1258 6 1461 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1463 10 0 0 1462 6 1461 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1464 4 0 0 990 6 1463 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1465 4 0 0 1354 6 1462 0 1464 0 0 0 0 1 0 0 0 0 0 0 0
A 1466 10 0 0 1463 6 1461 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1467 10 0 0 1466 6 1461 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1468 10 0 0 1467 6 1461 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1470 1 0 3 1367 3806 4284 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1471 10 0 0 1240 6 1470 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1472 10 0 0 1471 6 1470 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1473 4 0 0 994 6 1472 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1474 4 0 0 0 6 1471 0 1473 0 0 0 0 1 0 0 0 0 0 0 0
A 1475 10 0 0 1472 6 1470 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1476 10 0 0 1475 6 1470 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1477 10 0 0 1476 6 1470 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1479 1 0 3 393 3812 4290 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1480 10 0 0 1359 6 1479 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1481 10 0 0 1480 6 1479 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1482 4 0 0 1160 6 1481 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1483 4 0 0 0 6 1480 0 1482 0 0 0 0 1 0 0 0 0 0 0 0
A 1484 10 0 0 1481 6 1479 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1485 10 0 0 1484 6 1479 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1486 10 0 0 1485 6 1479 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1488 1 0 3 1251 3818 4296 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1489 10 0 0 303 6 1488 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1490 10 0 0 1489 6 1488 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1491 4 0 0 1122 6 1490 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1492 4 0 0 1308 6 1489 0 1491 0 0 0 0 1 0 0 0 0 0 0 0
A 1493 10 0 0 1490 6 1488 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1494 10 0 0 1493 6 1488 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1495 10 0 0 1494 6 1488 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1497 1 0 3 793 3824 4302 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1498 10 0 0 972 6 1497 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1499 10 0 0 1498 6 1497 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1500 4 0 0 0 6 1499 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1501 4 0 0 979 6 1498 0 1500 0 0 0 0 1 0 0 0 0 0 0 0
A 1502 10 0 0 1499 6 1497 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1503 10 0 0 1502 6 1497 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1504 10 0 0 1503 6 1497 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1506 1 0 3 1078 3830 4308 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1507 10 0 0 816 6 1506 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1508 10 0 0 1507 6 1506 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1509 4 0 0 1284 6 1508 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1510 4 0 0 713 6 1507 0 1509 0 0 0 0 1 0 0 0 0 0 0 0
A 1511 10 0 0 1508 6 1506 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1512 10 0 0 1511 6 1506 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1513 10 0 0 1512 6 1506 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1515 1 0 3 1393 3836 4314 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1516 10 0 0 1035 6 1515 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1517 10 0 0 1516 6 1515 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1518 4 0 0 1378 6 1517 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1519 4 0 0 1377 6 1516 0 1518 0 0 0 0 1 0 0 0 0 0 0 0
A 1520 10 0 0 1517 6 1515 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1521 10 0 0 1520 6 1515 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1522 10 0 0 1521 6 1515 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1524 1 0 3 1165 3842 4320 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1525 10 0 0 1168 6 1524 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1526 10 0 0 1525 6 1524 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1527 4 0 0 1117 6 1526 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1528 4 0 0 1222 6 1525 0 1527 0 0 0 0 1 0 0 0 0 0 0 0
A 1529 10 0 0 1526 6 1524 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1530 10 0 0 1529 6 1524 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1531 10 0 0 1530 6 1524 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1533 1 0 3 1208 3848 4326 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1534 10 0 0 1173 6 1533 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1535 10 0 0 1534 6 1533 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1536 4 0 0 1029 6 1535 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1537 4 0 0 859 6 1534 0 1536 0 0 0 0 1 0 0 0 0 0 0 0
A 1538 10 0 0 1535 6 1533 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1539 10 0 0 1538 6 1533 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1540 10 0 0 1539 6 1533 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1542 1 0 3 1540 3854 4332 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1543 10 0 0 0 6 1542 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1544 10 0 0 1543 6 1542 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1545 4 0 0 1536 6 1544 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1546 4 0 0 579 6 1543 0 1545 0 0 0 0 1 0 0 0 0 0 0 0
A 1547 10 0 0 1544 6 1542 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1548 10 0 0 1547 6 1542 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1549 10 0 0 1548 6 1542 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1551 1 0 3 1366 3860 4338 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1552 10 0 0 0 6 1551 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1553 10 0 0 1552 6 1551 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1554 4 0 0 0 6 1553 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1555 4 0 0 1550 6 1552 0 1554 0 0 0 0 1 0 0 0 0 0 0 0
A 1556 10 0 0 1553 6 1551 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1557 10 0 0 1556 6 1551 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1558 10 0 0 1557 6 1551 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1560 1 0 3 477 3866 4344 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1561 10 0 0 468 6 1560 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1562 10 0 0 1561 6 1560 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1563 4 0 0 0 6 1562 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1564 4 0 0 1147 6 1561 0 1563 0 0 0 0 1 0 0 0 0 0 0 0
A 1565 10 0 0 1562 6 1560 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1566 10 0 0 1565 6 1560 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1567 10 0 0 1566 6 1560 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1569 1 0 3 1249 3872 4350 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1570 10 0 0 1162 6 1569 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1571 10 0 0 1570 6 1569 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1572 4 0 0 0 6 1571 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1573 4 0 0 844 6 1570 0 1572 0 0 0 0 1 0 0 0 0 0 0 0
A 1574 10 0 0 1571 6 1569 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1575 10 0 0 1574 6 1569 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1576 10 0 0 1575 6 1569 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1585 1 0 3 1470 3921 4394 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1586 10 0 0 1094 6 1585 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1587 10 0 0 1586 6 1585 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1588 10 0 0 1587 6 1585 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1589 4 0 0 583 6 1588 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1590 4 0 0 1087 6 1587 0 1589 0 0 0 0 1 0 0 0 0 0 0 0
A 1591 10 0 0 1588 6 1585 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1592 10 0 0 1591 6 1585 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1593 1 0 3 1371 3927 4398 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1594 10 0 0 1006 6 1593 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1595 10 0 0 1594 6 1593 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1596 10 0 0 1595 6 1593 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1597 4 0 0 0 6 1596 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1598 4 0 0 100 6 1595 0 1597 0 0 0 0 1 0 0 0 0 0 0 0
A 1599 10 0 0 1596 6 1593 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1600 10 0 0 1599 6 1593 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1601 1 0 3 181 3933 4406 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1602 10 0 0 1375 6 1601 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1603 10 0 0 1602 6 1601 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1604 10 0 0 1603 6 1601 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1605 4 0 0 1515 6 1604 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1606 4 0 0 747 6 1603 0 1605 0 0 0 0 1 0 0 0 0 0 0 0
A 1607 10 0 0 1604 6 1601 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1608 10 0 0 1607 6 1601 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1609 1 0 3 1107 3939 4410 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1610 10 0 0 1016 6 1609 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1611 10 0 0 1610 6 1609 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1612 10 0 0 1611 6 1609 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1613 4 0 0 1185 6 1612 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1614 4 0 0 1049 6 1611 0 1613 0 0 0 0 1 0 0 0 0 0 0 0
A 1615 10 0 0 1612 6 1609 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1616 10 0 0 1615 6 1609 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1617 1 0 3 1206 3949 4438 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1618 10 0 0 1564 6 1617 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1619 10 0 0 1618 6 1617 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1620 10 0 0 1619 6 1617 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1621 4 0 0 1079 6 1620 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1622 4 0 0 416 6 1619 0 1621 0 0 0 0 1 0 0 0 0 0 0 0
A 1623 10 0 0 1620 6 1617 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1624 10 0 0 1623 6 1617 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1625 1 0 3 756 3955 4442 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1626 10 0 0 1032 6 1625 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1627 10 0 0 1626 6 1625 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1628 10 0 0 1627 6 1625 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1629 4 0 0 37 6 1628 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1630 4 0 0 526 6 1627 0 1629 0 0 0 0 1 0 0 0 0 0 0 0
A 1631 10 0 0 1628 6 1625 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1632 10 0 0 1631 6 1625 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1633 1 0 3 1447 3963 4452 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1634 10 0 0 1342 6 1633 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1635 10 0 0 1634 6 1633 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1636 10 0 0 1635 6 1633 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1637 4 0 0 1380 6 1636 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1638 4 0 0 978 6 1635 0 1637 0 0 0 0 1 0 0 0 0 0 0 0
A 1639 10 0 0 1636 6 1633 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1640 10 0 0 1639 6 1633 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1641 1 0 3 989 3969 4456 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1642 10 0 0 0 6 1641 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1643 10 0 0 1642 6 1641 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1644 10 0 0 1643 6 1641 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1645 4 0 0 65 6 1644 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1646 4 0 0 1641 6 1643 0 1645 0 0 0 0 1 0 0 0 0 0 0 0
A 1647 10 0 0 1644 6 1641 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1648 10 0 0 1647 6 1641 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1649 1 0 3 1161 3975 4475 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1650 10 0 0 1572 6 1649 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1651 10 0 0 1650 6 1649 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1652 10 0 0 1651 6 1649 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1653 4 0 0 81 6 1652 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1654 4 0 0 365 6 1651 0 1653 0 0 0 0 1 0 0 0 0 0 0 0
A 1655 10 0 0 1652 6 1649 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1656 10 0 0 1655 6 1649 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
A 1657 1 0 3 0 3981 4479 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 1658 10 0 0 0 6 1657 1 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 13
A 1659 10 0 0 1658 6 1657 4 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 314
A 1660 10 0 0 1659 6 1657 7 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 316
A 1661 4 0 0 1537 6 1660 0 3 0 0 0 0 2 0 0 0 0 0 0 0
A 1662 4 0 0 1171 6 1659 0 1661 0 0 0 0 1 0 0 0 0 0 0 0
A 1663 10 0 0 1660 6 1657 10 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 320
A 1664 10 0 0 1663 6 1657 13 0 0 0 0 0 0 0 0 0 0 0 0 0
X 1 200
Z
J 109 1 1
V 282 66 7 0
R 0 71 0 0
A 0 64 0 0 1 210 1
A 0 64 0 0 1 211 1
A 0 64 0 0 1 212 1
A 0 64 0 0 1 213 1
A 0 64 0 0 1 214 1
A 0 64 0 0 1 215 1
A 0 64 0 0 1 216 0
J 149 1 1
V 1057 1184 7 0
S 0 1184 0 0 0
A 0 6 0 0 1 2 0
J 150 1 1
V 1060 1193 7 0
S 0 1193 0 0 0
A 0 6 0 0 1 2 0
T 1118 174 0 3 0 0
A 1120 6 0 0 1 2 1
A 1123 7 185 0 1 2 0
T 1393 269 0 3 0 0
A 1394 6 0 0 1 2 1
A 1395 275 0 0 1 356 1
A 1396 6 0 0 1 2 1
A 1397 9 0 0 1 20 1
A 1398 9 0 0 1 20 0
T 1408 282 0 3 0 0
T 1409 269 0 3 0 1
A 1394 6 0 0 1 2 1
A 1395 275 0 0 1 356 1
A 1396 6 0 0 1 2 1
A 1397 9 0 0 1 20 1
A 1398 9 0 0 1 20 0
A 1410 6 0 0 1 2 1
A 1411 6 0 0 1 2 1
A 1415 7 291 0 1 2 0
T 1487 323 0 3 0 0
A 1488 6 0 0 1 2 1
A 1489 6 0 0 1 2 1
A 1490 6 0 0 1 2 0
T 1492 332 0 3 0 0
T 1493 323 0 15 0 0
A 1488 6 0 0 1 2 1
A 1489 6 0 0 1 2 1
A 1490 6 0 0 1 2 0
T 1518 368 0 3 0 0
A 1520 6 0 0 1 2 1
A 1521 6 0 0 1 2 1
A 1524 7 377 0 1 2 0
T 1854 733 0 3 0 0
A 1868 6 0 0 1 2 0
T 1942 827 0 3 0 0
T 1949 821 0 3 0 0
A 1868 6 0 0 1 2 0
T 2006 909 0 3 0 0
R 2007 915 0 1
A 0 6 0 200 1 2 0
R 2008 918 0 1
A 0 6 0 200 1 2 0
R 2009 921 0 1
A 0 6 0 200 1 2 0
A 2010 6 0 0 1 2 1
A 2011 9 0 0 1 20 1
A 2012 9 0 0 1 20 0
T 2020 927 0 3 0 0
T 2021 909 0 3 0 0
R 2007 915 0 1
A 0 6 0 200 1 2 0
R 2008 918 0 1
A 0 6 0 200 1 2 0
R 2009 921 0 1
A 0 6 0 200 1 2 0
A 2010 6 0 0 1 2 1
A 2011 9 0 0 1 20 1
A 2012 9 0 0 1 20 0
T 2038 942 0 0 0 0
A 2043 7 978 0 1 2 1
A 2050 7 980 0 1 2 1
A 2057 7 982 0 1 2 1
A 2064 7 984 0 1 2 1
A 2071 7 986 0 1 2 0
T 2084 991 0 3 0 0
A 2085 9 0 0 1 20 1
A 2086 16 0 0 1 969 0
T 2096 1014 0 3 0 0
T 2097 991 0 3 0 1
A 2085 9 0 0 1 20 1
A 2086 16 0 0 1 969 0
A 2098 6 0 0 1 2 1
A 2099 6 0 0 1 3 1
A 2100 9 0 0 1 20 1
A 2101 9 0 0 1 20 1
A 2102 6 0 0 1 3 1
A 2103 9 0 0 1 20 1
A 2104 9 0 0 1 20 1
A 2105 6 0 0 1 3 1
R 2106 1020 0 1
A 0 6 0 200 1 2 0
A 2114 9 0 0 1 20 1
A 2115 6 0 0 1 2 1
A 2116 16 0 0 1 969 0
T 2377 1041 0 3 0 0
A 2378 6 0 0 1 2 1
A 2379 1047 0 0 1 971 1
A 2382 7 1054 0 1 2 0
T 2385 1059 0 3 0 0
A 2386 1047 0 0 1 971 1
A 2387 1047 0 0 1 971 1
A 2390 7 1070 0 1 2 0
T 2677 1304 0 3 0 0
A 2678 1300 0 0 1 1106 1
A 2679 6 0 0 1 3 1
A 2680 9 0 0 1 20 1
A 2681 9 0 0 1 20 1
A 2682 9 0 0 1 20 1
A 2683 9 0 0 1 20 1
A 2684 6 0 0 1 3 1
R 2685 1310 0 1
A 0 9 0 49 1 20 0
A 2686 6 0 0 1 3 1
A 2687 9 0 0 1 20 1
A 2688 6 0 0 1 3 1
A 2689 9 0 0 1 20 1
A 2690 9 0 0 1 20 1
A 2691 9 0 0 1 20 1
A 2692 9 0 0 1 20 1
A 2693 6 0 0 1 3 1
A 2694 9 0 0 1 20 0
T 2731 1337 0 3 0 0
X 2 2732 6 0 0 1
X 7 0 6 0 0 0
L 7 0
A 0 6 0 0 1 3 1
A 0 6 0 0 1 13 0
A 0 6 0 0 1 3 0
A 2733 6 0 0 1 1107 1
X 7 2734 6 0 0 0
L 7 0
A 0 6 0 0 1 3 1
A 0 6 0 0 1 1107 0
T 2850 1476 0 3 0 0
T 2851 1461 0 3 0 1
A 2678 1457 0 0 1 1125 1
A 2679 6 0 0 1 3 1
A 2680 9 0 0 1 20 1
A 2681 9 0 0 1 20 1
A 2682 9 0 0 1 20 1
A 2683 9 0 0 1 20 1
A 2684 6 0 0 1 3 1
R 2685 1467 0 1
A 0 9 0 49 1 20 0
A 2686 6 0 0 1 3 1
A 2687 9 0 0 1 20 1
A 2688 6 0 0 1 3 1
A 2689 9 0 0 1 20 1
A 2690 9 0 0 1 20 1
A 2691 9 0 0 1 20 1
A 2692 9 0 0 1 20 1
A 2693 6 0 0 1 3 1
A 2694 9 0 0 1 20 0
A 2855 7 1485 0 1 2 1
A 2857 6 0 0 1 2 0
T 2933 1616 0 3 0 0
A 2934 1612 0 0 1 1171 1
A 2935 1612 0 0 1 1171 1
A 2936 6 0 0 1 3 1
A 2937 9 0 0 1 1172 1
A 2938 9 0 0 1 20 1
A 2939 9 0 0 1 20 1
A 2940 6 0 0 1 3 1
A 2941 1612 0 0 1 1171 1
A 2942 1612 0 0 1 1171 1
A 2943 6 0 0 1 3 1
A 2944 9 0 0 1 20 0
T 2979 1643 0 3 0 0
X 2 2980 6 0 0 1
X 7 0 6 0 0 0
L 7 0
A 0 6 0 0 1 3 1
A 0 6 0 0 1 1173 0
A 0 6 0 0 1 3 0
A 2981 6 0 0 1 311 1
X 7 2982 6 0 0 0
L 7 0
A 0 6 0 0 1 3 1
A 0 6 0 0 1 311 0
T 3103 1779 0 3 0 0
T 3104 1767 0 3 0 1
A 2934 1763 0 0 1 1183 1
A 2935 1763 0 0 1 1183 1
A 2936 6 0 0 1 3 1
A 2937 9 0 0 1 1172 1
A 2938 9 0 0 1 20 1
A 2939 9 0 0 1 20 1
A 2940 6 0 0 1 3 1
A 2941 1763 0 0 1 1183 1
A 2942 1763 0 0 1 1183 1
A 2943 6 0 0 1 3 1
A 2944 9 0 0 1 20 0
A 3108 7 1788 0 1 2 1
A 3110 6 0 0 1 2 0
T 3212 1889 0 3 0 0
A 3213 6 0 0 1 2 1
A 3214 6 0 0 1 3 1
A 3215 6 0 0 1 3 1
A 3216 6 0 0 1 3 1
R 3217 1898 0 1
A 0 6 0 0 1 1209 1
A 0 6 0 0 1 1210 0
R 3218 1904 0 1
A 0 6 0 0 1 3 1
A 0 6 0 0 1 3 1
A 0 6 0 0 1 3 0
A 3219 6 0 0 1 3 1
A 3220 6 0 0 1 1211 1
A 3221 6 0 0 1 13 1
A 3222 6 0 0 1 2 1
A 3223 6 0 0 1 51 1
A 3224 9 0 0 1 1212 1
A 3225 9 0 0 1 1213 1
A 3226 6 0 0 1 2 1
A 3227 6 0 0 1 3 1
A 3228 6 0 0 1 1214 1
A 3229 6 0 0 1 2 1
A 3236 6 0 0 1 2 1
A 3237 6 0 0 1 3 1
A 3238 8 0 0 1 1215 1
A 3239 6 0 0 1 2 1
A 3240 6 0 0 1 51 1
A 3241 6 0 0 1 2 1
A 3242 6 0 0 1 53 1
R 3243 1913 0 1
A 0 16 0 200 1 1216 0
A 3244 16 0 0 1 1216 1
A 3245 6 0 0 1 2 1
A 3246 6 0 0 1 53 1
A 3247 6 0 0 1 53 1
A 3248 6 0 0 1 2 1
A 3249 6 0 0 1 53 1
A 3250 6 0 0 1 2 1
A 3251 6 0 0 1 53 1
A 3254 7 1927 0 1 2 1
A 3258 7 1929 0 1 2 1
A 3260 1875 0 0 1 1208 1
A 3261 1875 0 0 1 1208 1
A 3262 1875 0 0 1 1208 1
A 3263 1875 0 0 1 1208 1
A 3264 1875 0 0 1 1208 1
A 3265 1875 0 0 1 1208 1
A 3266 6 0 0 1 1217 1
A 3267 6 0 0 1 1217 1
A 3268 6 0 0 1 2 1
A 3269 6 0 0 1 1217 1
A 3270 6 0 0 1 1217 1
A 3271 6 0 0 1 2 1
A 3272 6 0 0 1 1217 1
A 3273 6 0 0 1 1217 1
A 3274 6 0 0 1 2 1
A 3275 6 0 0 1 1217 1
A 3276 6 0 0 1 1217 1
A 3277 6 0 0 1 2 1
A 3278 16 0 0 1 969 1
A 3281 7 1931 0 1 2 0
T 3413 2062 0 3 0 0
A 3417 7 2071 0 1 2 1
A 3419 6 0 0 1 2 0
T 3435 2076 0 3 0 0
A 3436 6 0 0 1 2 1
A 3437 6 0 0 1 2 1
R 3438 2082 0 1
A 0 9 0 18 1 20 0
A 3439 6 0 0 1 2 1
A 3440 9 0 0 1 20 1
A 3441 9 0 0 1 20 1
A 3442 9 0 0 1 20 1
A 3443 9 0 0 1 20 1
A 3444 2027 0 0 1 1246 0
T 3573 2413 0 3 0 0
T 3574 2337 0 3 0 1
X 2 2732 6 0 0 1
X 7 0 6 0 0 0
L 7 0
A 0 6 0 0 1 3 1
A 0 6 0 0 1 13 0
A 0 6 0 0 1 3 0
A 2733 6 0 0 1 1107 1
X 7 2734 6 0 0 0
L 7 0
A 0 6 0 0 1 3 1
A 0 6 0 0 1 1107 0
T 3575 2370 0 3 0 1
X 2 2980 6 0 0 1
X 7 0 6 0 0 0
L 7 0
A 0 6 0 0 1 3 1
A 0 6 0 0 1 1173 0
A 0 6 0 0 1 3 0
A 2981 6 0 0 1 311 1
X 7 2982 6 0 0 0
L 7 0
A 0 6 0 0 1 3 1
A 0 6 0 0 1 311 0
T 3576 2256 0 3 0 1
T 1949 2250 0 3 0 0
A 1868 6 0 0 1 2 0
T 3577 2404 0 3 0 1
A 3436 6 0 0 1 2 1
A 3437 6 0 0 1 2 1
R 3438 2410 0 1
A 0 9 0 18 1 20 0
A 3439 6 0 0 1 2 1
A 3440 9 0 0 1 20 1
A 3441 9 0 0 1 20 1
A 3442 9 0 0 1 20 1
A 3443 9 0 0 1 20 1
A 3444 2394 0 0 1 1362 0
A 3578 9 0 0 1 1363 1
R 3579 2419 0 1
A 0 6 0 207 1 2 0
R 3580 2422 0 1
A 0 6 0 49 1 2 0
R 3581 2425 0 1
A 0 6 0 49 1 2 0
A 3582 6 0 0 1 2 1
T 3583 2145 0 3 0 1
A 1394 6 0 0 1 2 1
A 1395 2151 0 0 1 1362 1
A 1396 6 0 0 1 2 1
A 1397 9 0 0 1 20 1
A 1398 9 0 0 1 20 0
A 3587 7 2437 0 1 2 1
T 3589 2283 0 3 0 1
A 2043 7 2289 0 1 2 1
A 2050 7 2291 0 1 2 1
A 2057 7 2293 0 1 2 1
A 2064 7 2295 0 1 2 1
A 2071 7 2297 0 1 2 0
T 3590 2277 0 3 0 1
T 2021 2262 0 3 0 0
R 2007 2268 0 1
A 0 6 0 200 1 2 0
R 2008 2271 0 1
A 0 6 0 200 1 2 0
R 2009 2274 0 1
A 0 6 0 200 1 2 0
A 2010 6 0 0 1 2 1
A 2011 9 0 0 1 20 1
A 2012 9 0 0 1 20 0
A 3594 7 2439 0 1 2 1
A 3599 7 2441 0 1 2 0
T 3937 3309 0 3 0 0
A 3939 9 0 0 1 20 1
A 3940 6 0 0 1 2 1
A 3941 6 0 0 1 2 1
A 3942 6 0 0 1 2 1
A 3943 9 0 0 1 20 1
A 3944 6 0 0 1 2 1
A 3945 6 0 0 1 2 1
A 3946 6 0 0 1 2 1
A 3947 6 0 0 1 2 1
A 3948 9 0 0 1 20 1
A 3949 9 0 0 1 20 1
T 3938 3017 0 3 0 0
T 2097 3011 0 3 0 1
A 2085 9 0 0 1 20 1
A 2086 16 0 0 1 969 0
A 2098 6 0 0 1 2 1
A 2099 6 0 0 1 3 1
A 2100 9 0 0 1 20 1
A 2101 9 0 0 1 20 1
A 2102 6 0 0 1 3 1
A 2103 9 0 0 1 20 1
A 2104 9 0 0 1 20 1
A 2105 6 0 0 1 3 1
R 2106 3023 0 1
A 0 6 0 200 1 2 0
A 2114 9 0 0 1 20 1
A 2115 6 0 0 1 2 1
A 2116 16 0 0 1 969 0
T 4205 3749 0 3 0 0
A 4206 9 0 0 1 1172 1
A 4207 9 0 0 1 1396 1
A 4209 9 0 0 1 1172 1
A 4210 9 0 0 1 1396 1
A 4211 9 0 0 1 1172 1
A 4212 9 0 0 1 1396 1
A 4213 9 0 0 1 1172 1
A 4214 9 0 0 1 1172 1
A 4215 9 0 0 1 1172 1
A 4216 9 0 0 1 1172 1
A 4217 9 0 0 1 1172 1
A 4218 9 0 0 1 1172 1
A 4219 9 0 0 1 1172 1
A 4220 9 0 0 1 1396 1
A 4221 9 0 0 1 1396 1
A 4222 9 0 0 1 1396 1
A 4223 9 0 0 1 1396 1
A 4224 9 0 0 1 1396 1
A 4225 9 0 0 1 1396 1
A 4226 9 0 0 1 1396 1
A 4227 9 0 0 1 20 1
A 4228 9 0 0 1 20 1
A 4229 9 0 0 1 20 1
A 4230 9 0 0 1 20 1
A 4231 9 0 0 1 20 1
A 4232 9 0 0 1 20 1
A 4233 9 0 0 1 20 1
A 4354 6 0 0 1 2 1
A 4355 6 0 0 1 2 1
A 4356 6 0 0 1 2 1
A 4357 6 0 0 1 2 1
A 4358 6 0 0 1 2 1
A 4359 6 0 0 1 2 1
A 4360 6 0 0 1 2 1
A 4361 6 0 0 1 2 1
A 4362 6 0 0 1 2 1
A 4363 6 0 0 1 2 1
A 4364 6 0 0 1 2 0
Z
