cmake_minimum_required(VERSION 2.8.3)

project(%(pkgName)s)

find_package(catkin REQUIRED COMPONENTS knowrob_common)

catkin_package(
    DEPENDS knowrob_common
)
