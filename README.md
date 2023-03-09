# RFC 3339 date Parse/Construct for Gauche

Construct and Parse like followings format:

- 2014-01-02T03:04:05Z
- 2014-01-02T03:04:05+09:00
- 2020-04-12 23:20:50

Other name `ISO date` `ISO 8601`

# Docker

**Now testing**

## Building

```
docker run -v `pwd`:/home/app --rm -ti practicalscheme/gauche sh -c 'cd /home/app && ./configure && make check'
```

## Cleanup

cleanup dirty working copy.

```
docker run -v `pwd`:/home/app --rm -ti practicalscheme/gauche sh -c 'cd /home/app && make distclean'
```
