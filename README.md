# Easync
===

Simple HTTP File Sync API

## Requirements

* Stack
* Redis

## Installation

```
git clone https://gitlab.com/theSuess/easync.git
cd easync
stack install
# start the redis server then:
easync
```

## Usage

```
# Create a new User
curl -i -X POST -H "Username:Rick" -H "Password:Sanchez" localhost:3000/user/create

# Upload a file
curl -i -X POST -F "file=@/path/to/your/file" \
    -H "Username:Rick" -H "Password:Sanchez" \
    localhost:3000/sync/filename

# Get the file
curl -i -H "Username:Rick" -H "Password:Sanchez" localhost:3000/sync/filename
```
