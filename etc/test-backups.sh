#! /bin/bash

# This is a simple test to verify that the backups and the cache work.
# Ideally, this should be incorporated into the main tests (and be
# able to know if it is correct).

# It requires 'src' point to a git tree (in this case, of the git
# source itself), with two tags into that repo that will make changes.
# The gosure tool helps determine what is wrong.  Git is pretty good
# at know something is wrong, but won't indicate what very easily.

# Experiments in backup.
odump=./odump.native
pool=/tmp/pool
src=~/scm/git
srcrev1=v1.7.7
srcrev2=v1.7.8
dest=/tmp/restore

# src=/tmp/simple

# Backup base
rm -rf $pool
mkdir $pool
$odump create-pool $pool

(cd $src; git checkout --quiet $srcrev1; gosure scan)
$odump dump $pool $src phase=1
hash=$($odump list $pool | tail -1 | awk '{print $1;}')
rm -rf $dest
mkdir $dest
$odump restore $pool $hash $dest

# This sleep shouldn't be needed if we are really storing the fine-grained time.
sleep 1   # Needed to make sure times advance.

(cd $dest; gosure check; git status; git --no-pager log -1 --oneline)

# TESTA: remove the seen database, and recreate it.
rm $pool/seen/*.sqlite
$odump make-cache $pool $hash $src

# Phase 2, different branch.
(cd $src; git checkout --quiet $srcrev2; gosure update)
$odump dump $pool $src phase=1
hash=$($odump list $pool | tail -1 | awk '{print $1;}')
rm -rf $dest
mkdir $dest
$odump restore $pool $hash $dest

echo ''
echo 'Checking restore'
(cd $dest; gosure check; git status; git --no-pager log -1 --oneline)
