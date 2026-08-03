# Energy Data Analysis

Joint project by [Johannes Schmidt](https://github.com/joph) and me. Provides data loading, preparation and analysis scripts. These script are also the base for the visualisations on [energy.abteil.org](https://energy.abteil.org).

## Data sources

Following data sources are used:

- [E-Control](https://www.e-control.at/)
    - Gas consumption
- [entsoe](https://www.entsoe.eu/)
    - Electricity generation
    - Electricity load
- [GIE - Gas Infrastructure Europe](https://www.gie.eu/)
    - Gas Storage
- [AGGM - Austrian Gas Grid Managment AG](https://www.aggm.at/)
    - Gas Consumption
- [CDS - Climate Data Store](https://cds.climate.copernicus.eu/)
    - Temperature
- [EEX - European Energy Exchange](https://www.eex.com/) via [Macrobond](https://www.macrobond.com/)
    - Electricity price
- [ICE - Intercontinental Exchange](https://www.theice.com/) via [Macrobond](https://www.macrobond.com/)
    - Coal Price
    - Brent Price
    - EUA Price
- [NASDAQ OMX - Nasdaq Commodities ](http://www.nasdaqomx.com/) via [Macrobond](https://www.macrobond.com/)
    - EUA Price (discontinued, Nasdaq Commodities withdrew all futures on 30.04.2026, last observation 06.01.2026, kept as `price-eua-nasdaq`)
- others

## Contribute

Any contribution is welcome, start by cloning this repo:
```
    git clone https://github.com/energy-monitor/explore
```

Install dependencies with :
```bash
    cd explore
    pixi shell
```


## Loading Data

Copy the `creds-template.json` file to `creds.json` and fill it with your credentials. Credentials can be obtained by registering on the corresponding data supplier webpages (free, except for data series fetched via the data provider Macrobond).

Calling one of the scripts in the `load` folder will download the data from the corresponding data source, extract, aggregate and store the data for the visualisation.

## Storage

Prepared data sets are written by `saveToStorages()` and read back by `loadFromStorage()` (see `_storage.r`). Three storage types are implemented:

| Type | Configured in | Setting | Description |
| --- | --- | --- | --- |
| `local` | `config.json` | `path` | Directory below the project root. |
| `googledrive` | `config.json` | `path` | Folder on Google Drive. |
| `sftp` | `creds.json` | `host`, `port`, `user`, `path`, `keyfile`, `knownHosts`, `keypass` | Remote directory on an SFTP server. |

`storage.default.load` in `config.json` selects the type to read from, `storage.default.save` lists the types to write to.

The settings of the `sftp` type are kept in the `sftp` section of `creds.json` instead of `config.json`, so that the server does not end up in the repository. It authenticates with a public key only, no passwords. Point `keyfile` at the private key, the matching `.pub` file is picked up automatically if it exists. A `path` that is not absolute is taken relative to the login home directory. `keypass` is only needed if the private key is protected by a passphrase.

The host key is verified against the file given in `knownHosts`, so the server needs an entry there before the first transfer:

```bash
    ssh-keyscan -p <port> <host> >> ~/.ssh/known_hosts
```

Compare the fingerprint with the one on the server (`ssh-keygen -lf /etc/ssh/ssh_host_ed25519_key.pub`) before trusting it. Setting `knownHosts` to an empty string disables the check entirely, which is not recommended.

