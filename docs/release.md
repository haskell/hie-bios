# Release Checklist

This document described the release process for `hie-bios`.

* [ ] Bump `version` in `hie-bios.cabal` [compliant with PVP](https://pvp.haskell.org/)
* [ ] Update ChangeLog.md to include all changes.
* [ ] GitHub CI must be green
* [ ] Upload release candidate
  * `cabal sdist; cabal upload dist-newstyle/sdist/hie-bios-<version>.tar.gz`
* [ ] Upload release candidate documentation
  * `cabal haddock --haddock-for-hackage; cabal upload --documentation dist-newstyle/hie-bios-<version>-docs.tar.gz`
* [ ] Manually verify the docs render correctly
* [ ] Upload release
  * `cabal upload --publish dist-newstyle/sdist/hie-bios-<version>.tar.gz; cabal upload --publish --documentation dist-newstyle/hie-bios-<version>-docs.tar.gz`
* [ ] Create release tag for GitHub
  * [ ] `git tag -a release-<version>`
  * [ ] `git push --tags`
