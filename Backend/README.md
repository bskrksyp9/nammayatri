
This is the sub-project containing backend code written in [haskell] powering [nammayatri] servers.

## Getting Started

### Pre-requisites

To build or develop the project, you need to install the following.

#### Nix

Nix is central to building and developing the Namamayatri project. To prepare your system for a pleasant Nix-based development, follow these three steps:

1. [Install **Nix**](https://github.com/DeterminateSystems/nix-installer#the-determinate-nix-installer)
    - If you already have Nix installed, you must [enable Flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes) manually.
    - Then, run the following to check that everything is green ✅.
        ```sh
        nix run github:srid/nix-health
        ```
1. Setup the Nix **binary cache** (to avoid compiling locally for hours):
    ```sh
    nix run nixpkgs#cachix use nammayatri
    ```
    - For this command to succeed, you must have added yourself to the `trusted-users` list of `nix.conf`
1. Install **home-manager**[^hm] and setup **nix-direnv** and **starship** by following the instructions [in this home-manager template](https://github.com/juspay/nix-dev-home).[^direnv] [You want this](https://haskell.flake.page/direnv) to facilitate a nice Nix develoment environment.

[^hm]: Unless you are using NixOS in which case home-manager is not strictly needed.
[^direnv]: Not strictly required to develop nammayatri. If you do not use `direnv` however you would have to remember to manually restart the `nix develop` shell, and know when exactly to do this each time.

#### Other tools

Aside from Nix, you also need to:

1. Install [Docker](https://www.docker.com/products/docker-desktop/) (we use [arion]--a `docker-compose` invoker in Nix--for running external service dependencies).
    - If you are on macOS, open *Docker -> Preferences... -> Resources -> File Sharing* in Docker Desktop and add `/nix/store` to the list of shared folders. ![image](https://user-images.githubusercontent.com/3998/235455381-f88466b7-ee29-4baf-b0a9-4ddcf54ba402.png)

1. Install [Xcode](https://developer.apple.com/xcode/), if you are on macOS.


### Building

**🚨 Attention 🚨**: You do not need to run `nix build` if you are developing the project locally (`nix build` is run in CI). Skip to the [Development](#development) section below. You should prefer [cabal] over Nix when building locally because cabal is faster.

Once you have installed all the necessary tools and dependencies, we can proceed with building the project for development.

To compile the backend, use the following command:

```sh
nix build .#nammayatri
```

This should produce a `./result` symlink in the current directory, containing all backend binaries under `./result/bin`.

**🚧 Warning 🚧**: The `nix build` command should _only_ build the nammayatri project and it should finish in a matter of minutes. If not, you must not have setup the Nix cache properly. Consult [the steps further above](#nix).

#### Building the docker image

```sh
docker load -i $(nix build .#dockerImage --no-link --print-out-paths)
```

### Development

#### Setting up a development environment

**🚨 Attention 🚨**: If you were using *stack* to develop Nammayatri in the past, you must **completely erase** that git working copy, and start from a fresh clone of this repository before working with Nix. You might also want to remove your cache folders `~/.cache/cabal` and `~/.cabal/hie-bios`.

To set up your development environment, you should run `direnv allow`[^de-ns] from the project root. If you do not have nix-direnv setup (as per the pre-requisites above), run instead:

[^de-ns]: If you are not using `direnv` and if you know what you are doing, you could manually start the [nix shell][nix-shell] using `nix develop`.

```sh
cd ~/Projects/nammayatri
direnv allow   # Need to run this only once
```

**🚧 Warning 🚧**: Entering the nix develop shell (using `direnv allow`, for example) should not compile anything and it should finish in a matter of minutes (after downloading the binaries from nammayatri.cachix.org). If not, you must not have setup the Nix cache properly. Consult [the steps further above](#nix).

This will drop you into a [shell environment][nix-shell] containing all project dependencies. Inside the nix shell, run `,` to see the available commands specific to nammayatri development.

To compile the project, use [cabal]:

```sh
# Do this in nix develop shell activated direnv:
cd ./Backend
# Build all packages
cabal build all
# Run a cabal package (by path to the directory containing .cabal file)
cabal run lib/location-updates
# Run ghcid (for fast compile feedback)
, ghcid lib/location-updates
```

#### Running external services

To run the project, we'd first need to run some services. These are provided via docker images, that are built in Nix and run via [arion].

For running the database, redis, passetto and kafka run this command:

```sh
# NOTE: You must run this from inside nix develop shell.
, run-svc
```

That should run most of the services required.

More services, if needed, can be run with the following commands.

For running pgadmin run this command:

```sh
, run-pgadmin
```

For running monitoring services like prometheus and grafana use this command:

```sh
, run-monitoring
```

To run osrm-server (which is not using Docker), run:

```sh
nix run .#osrm-server
```

#### Running backend services

To run the backend either use:

```sh
# From nix develop shell
, run-mobility-stack-dev
```

This will run the mobility stack using `cabal run` which is the recommend approach for development environment.

You can also use Nix to run the mobility stack, but this is slower compared to the cabal way because it involves doing a full Nix build.

```sh
, run-mobility-stack-nix
# Or (if you are not in the git repo):
nix run github:nammayatri/nammyatri#run-mobility-stack-nix
```

#### Updating flake inputs

External dependencies of the project are usually specified in [`inputs`](https://nixos.wiki/wiki/Flakes#Input_schema) attribute of the `flake.nix` file. They usually point to external Git repos, but they can also point to local directories (which is useful during development)

The specific revisions of these Git repos are pinned in the `flake.lock` file. If you want to update, say, shared-kernel to a particular commit, run:

```sh
nix flake lock --update-input shared-kernel --override-input shared-kernel github:nammayatri/shared-kernel/f8a79646de474d1a103365e27677778f278b100f
```

If you just want to advance the pinned commit to the HEAD of the `main` branch, run instead:

```sh
nix flake lock --update-input shared-kernel
```

You can also change the flake input to point a local checkout. To do this, change the `flake.nix` to be like:

```nix
{
  inputs = {
    shared-kernel.url = "path:/Users/myname/Projects/shared-kernel";
  };
}
```

Now, if you run `nix build` or any of the other nix commands, it will use the local shared-kernel to compile nammayatri against. Whenever you change the contents of `/Users/myname/Projects/shared-kernel`,  you **must** run `nix flake lock --update-input shared-kernel` again, so the current project will use the new contents.

#### Visual Studio Code

Once you have cloned the repo and have been successfully able to build the project using `cabal build all`, you can use [Visual Studio Code](https://code.visualstudio.com/) to develop the project.

- Launch [VSCode](https://code.visualstudio.com/), and open the `git clone`’ed project directory [as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces)
    - NOTE: If you are on Windows, you must use the [Remote - WSL extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-wsl) to open the folder in WSL.
- When prompted by VSCode, install the [workspace recommended](https://code.visualstudio.com/docs/editor/extension-marketplace#_workspace-recommended-extensions) extensions.
    - If it doesn’t prompt, press Cmd+Shift+X and search for `@recommended` to install them all manually.
- Ensure that the direnv extension is fully activated. You should expect to see this in the footer of VSCode: ![image](https://user-images.githubusercontent.com/3998/235459201-f0442741-294b-40bc-9c65-77500c9f4f1c.png)
- Once direnv is activated (and only then) open a Haskell file (`.hs`). You should expect haskell-language-server to startup, as seen in the footer: ![image](https://user-images.githubusercontent.com/3998/235459551-7c6c0c61-f4e8-41f3-87cf-6a834e2cdbc7.png)
    - Once this processing is complete, all IDE features should work.

### Testing

The project comes with a range of tests in its test-suites. These tests should pass for each correct build.

To run the test-suite for the project,

- first ensure you have the services running (see [running external services section](#running-external-services)).
- Then, run the osrm-server using `nix run .#osrm-server`

Run the following command in `./Backend` folder after the services are up and running:

```sh
cabal test all
```


## Usage

Each of the application has particular set of defined APIs and Schemas. To get available APIs/Schemas of any particular application, follow these steps

1. Copy the swagger json from `http://localhost:<port>/swagger` and use the relevant port (enlisted below)

| Application                              | Port   |
| -----------------------------------------|--------|
| rider-app                                | `8013` |
| static-offer-driver-app                  | `8014` |
| beckn-gateway                            | `8015` |
| dynamic-offer-driver-app                 | `8016` |
| mock-registry                            | `8020` |
| transporter-scheduler                    | `8053` |
| allocation-service                       | `9996` |

2. To run the requests one can use the Postman or any other API platform.

## Project Structure

The top level of the project has a very descriptive folder structure with helpful names.

The entire project is structured as a collection of smaller focused packages, which can be found listed in the top level `cabal.project` file, under the _packages_ section.

Each package has clear separation of focuses w.r.t the functionality it provides, which helps with maintenance and development and provides clear designated areas to look at for a specific desired behavior and functionality. A good overview of the app structure be found in the table below:-

```text
├── rider-platform                                  : encapsulates all the rider side microservices
|   ├── rider-app (rider-app-exe)                   : Frontend facing APIs, rider app
|   └── public-transport
|       ├── Main (public-transport-rider-platform-exe)
|       └── search-consumer	(public-transport-search-consumer-exe)
├── provider-platform                               : encapsulates all the provider side microservices
|   ├── static-offer-driver-app                     : Microservices that power fixed price ride
|   |   |                                             hailing service
|   |   ├── Allocator (allocation-service-exe)      : Allocation service that matches a driver
|   |   |                                             to a ride
|   |   ├── Main (static-offer-driver-app-exe)      : Frontend facing APIs, driver app
|   |   └── Scheduler (transporter-scheduler-exe)   : Job scheduler for scheduling rental rides
|   ├── dynamic-offer-driver-app                    : Microservices that power dynamic pricing,
|   |   |                                             as quoted by the driver, service
|   |   ├── Allocator (driver-offer-allocator-exe)  : Allocation service that matches a driver
|   |   |                                             to a ride
|   |   └── Main (dynamic-offer-driver-app-exe)     : Frontend facing APIs, driver app
|   ├── driver-tracking-health-check
├── dashboard
|   ├── rider-dashboard (rider-dashboard-exe)       : Rider specific ops dashboard APIs
|   └── provider-dashboard (provider-dashboard-exe) : Provider specific ops dashboard APIs
├── kafka-consumers                                 : Microservices that consume messages from kafka
|                                                     to perform various tasks
├── mocks                                           : Mock servers that mock various
|                                                     third party APIs, used for local testing
└── utils
    ├── image-api-helper (image-api-helper-exe)
    └── route-extractor	(route-extractor-exe)
```

## FAQs

1. I can't figure out the project structure.

    Please refer to the [Project Structure Section](#project-structure)

1. In Visual Studio Code Terminal, I get the error ```Assertion `path != ""' failed.```.

    This appears to be [a bug in the VSCode direnv extension](https://github.com/NixOS/nix/issues/6409#issuecomment-1407799718). Run `unset NIX_STORE` in the terminal to fix it.

1. How to find out where a library dependency is specified in Nix?

    Run `nix run github:nix-commmunity/nix-melt` to navigate and find that transitive flake input specifying the dependency you are looking for. You can also inspect the `flake.lock` file.

1. TBD...

[nammayatri]: https://www.nammayatri.in/
[haskell]: https://www.haskell.org/
[arion]: https://github.com/hercules-ci/arion
[cabal]: https://cabal.readthedocs.io/
[nix-shell]: https://nixos.wiki/wiki/Development_environment_with_nix-shell
