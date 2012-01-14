#! /bin/sh -e

tmp=/tmp/atdgen-$(date +%s)
mkdir $tmp
cd $tmp
git clone git@github.com:MyLifeLabs/atdgen.git
cd atdgen
make VERSION
version=$(cat VERSION)
make doc
rm -rf release/atdgen-$version
mkdir -p release/atdgen-$version
mkdir -p release/atdgen-$version/odoc
mkdir -p release/atdgen-$version/manual
mkdir -p release/atdgen-$version/example
find `git ls-files | grep -v 'manual\|example'` \
  | cpio -pvd release/atdgen-$version
cd manual
find `git ls-files` | cpio -pvd ../release/atdgen-$version/manual
cd ..
cp manual/atdgen-manual.pdf release/atdgen-$version/manual
cp manual/atdgen-manual.txt release/atdgen-$version/manual
cp manual/atdgen-manual.html release/atdgen-$version/manual
find odoc/* | cpio -pvd release/atdgen-$version
cd example
find `git ls-files` | cpio -pvd ../release/atdgen-$version/example
cd ..
cd release && tar czf atdgen-$version.tar.gz atdgen-$version
echo "Release files:
$tmp/atdgen/release/atdgen-$version
$tmp/atdgen/release/atdgen-$version.tar.gz"
