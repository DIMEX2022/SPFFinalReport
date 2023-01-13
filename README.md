# SPFFinalReport
Repository for the case study and report for the final SPF reporting period

# Fork usage

This is a fork of an upstream repository, the recommended workflow is to
submit all pull requests against `development` branch of this repository.

Once `development` reaches a stable point it will be submitted as a pull
request to the upstream repository.

## Local repository configuration

The best practice when working with this fork is to use
`development` branch as if it were `main`. The following
commands configure your local repository to make it
easier to always be working relative to `development`.

First, check out a local `development` branch tracking
the GitHub `origin/development` branch.

```
git switch development
```

Then, remove the local `main` branch to prevent accidental
muscle memory or command history completion errors.

```
git branch -d main
```

Finally, you are now ready to begin working on a new topic branch.

```
git checkout -b topic
```

## Submitting a Pull Request (PR)

When submitting a pull request remember to specify `informatics-lab/DIMEX` as the repo
and `development` as the branch.

# DIMEX

Data Integration Model for Exposures (DIMEX)

## Docker

To build DIMEX with Docker run the following command.

```sh
docker build -t dimex .
```

Then, to run the containerised application.

```sh
docker run dimex
```
