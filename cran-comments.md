## Test environments
* local OS Linux Mint 18.3 install, R 3.5.1
* ubuntu 14.04 (on travis-ci), R 3.4.4
* win-builder (devel and release)

## R CMD check results local

0 errors | 0 warnings | 0 note

## R CMD check results win-builder

0 errors | 0 warnings | 2 note

### Resubmission
This is a resubmission. This is a resubmission. The reason is: 
* The notes from win-builder are relateds example time.
* The reason of this time is because à call to api, where this package connect.

In this version I have:
* Wrap examples in \donttest{}.
