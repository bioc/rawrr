#!/bin/bash
set -euxo pipefail

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <project_folder> <output_folder>"
    exit 1
fi
project_folder="$1"
output_folder="$2"
work_folder="/tmp/build"

# debug: 
#apt-get update && apt-get upgrade -y && apt-get install -y zip tree

# Checkout the deps
mkdir /tmp/build-dir && cd /tmp/build-dir
git clone --depth=1 https://github.com/thermofisherlsms/RawFileReader.git
dotnet nuget add source "$PWD"/RawFileReader/Libs/NetCore/Net8/

# Perform the release
cp -r "$project_folder" "$work_folder"
cd "$work_folder"
runtimes="linux-x64 osx-x64 win-x64"
for runtime in $runtimes; do
    dotnet publish --runtime "$runtime" -c Release
done

# debug:
#tree bin/Release/net8.0
mkdir -p "$output_folder"

# Export the result
for runtime in $runtimes; do
    source_path="bin/Release/net8.0/$runtime/publish"
    suffix=""
    if [[ "$runtime" == win-* ]]; then
        suffix=".exe"
    fi
    cp "$source_path/rawrr$suffix" "$output_folder/rawrr-$runtime$suffix"
done
