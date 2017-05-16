#! /bin/sh -e

tmp=/tmp/atd-$(date +%s)
mkdir $tmp
cd $tmp
git clone git@github.com:MyLifeLabs/atd.git
cd atd
make VERSION
version=$(cat VERSION)
make doc
rm -rf release/atd-$version
mkdir -p release/atd-$version
mkdir -p release/atd-$version/odoc
mkdir -p release/atd-$version/manual
find `git ls-files | grep -v manual` | cpio -pvd release/atd-$version
cd manual
find `git ls-files` | cpio -pvd ../release/atd-$version/manual
cp atd-manual.pdf ../release/atd-$version/manual
cp atd-manual.txt ../release/atd-$version/manual
cp atd-manual.html ../release/atd-$version/manual
cd ..
find odoc/* | cpio -pvd release/atd-$version
cd release && tar czf atd-$version.tar.gz atd-$version
echo "Release files:
$tmp/atd/release/atd-$version
$tmp/atd/release/atd-$version.tar.gz"
