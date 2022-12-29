# SPFFinalReport
Repository for the case study and report for the final SPF reporting period

# Fork usage

This is a fork of an upstream repository, the recommended workflow is to
submit all pull requests against `development` branch of this repository.

Once `development` reaches a stable point it will be submitted as a pull
request to the upstream repository.

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
