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
- [ASFINAG](https://www.asfinag.at/verkehr-sicherheit/verkehrszaehlung/)
    - Traffic on motorways and expressways
- [Eurostat](https://ec.europa.eu/eurostat/)
    - Rail transport (goods quarterly, passengers annual)
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

## Configuration

`config.json` holds the defaults shared by all machines. Settings specific to a machine, like the storage to use or the path to the web project, go into `config.local.json` (not tracked), which overrides `config.json`. Objects are merged, all other values (incl. arrays) are replaced, e.g.:

```json
{
    "storage": {
        "default": {
            "load": "sftp",
            "save": ["local", "sftp"]
        }
    }
}
```

## Storage

Prepared data sets are written by `saveToStorages()` and read back by `loadFromStorage()` (see `_storage.r`). Two storage types are implemented:

| Type | Configured in | Setting | Description |
| --- | --- | --- | --- |
| `local` | `config.json` | `path` | Directory below the project root. |
| `sftp` | `creds.json` | `host`, `port`, `user`, `path`, `keyfile`, `knownHosts`, `keypass` | Remote directory on an SFTP server. |

`storage.default.load` selects the type to read from, `storage.default.save` lists the types to write to (both `local` by default, see [Configuration](#configuration)).

The settings of the `sftp` type are kept in the `sftp` section of `creds.json` instead of `config.json`, so that the server does not end up in the repository. It authenticates with a public key only, no passwords. Point `keyfile` at the private key, the matching `.pub` file is picked up automatically if it exists. A `path` that is not absolute is taken relative to the login home directory. `keypass` is only needed if the private key is protected by a passphrase.

The host key is verified against the file given in `knownHosts`, so the server needs an entry there before the first transfer. Use a dedicated file rather than appending to your regular `~/.ssh/known_hosts`:

```bash
    ssh-keyscan -p <port> <host> > ~/.ssh/known_hosts_sftp
```

Compare the fingerprint with the one on the server (`ssh-keygen -lf /etc/ssh/ssh_host_ed25519_key.pub`) before trusting it. Setting `knownHosts` to an empty string disables the check entirely, which is not recommended.

The entries in this file must not be **hashed**, otherwise the connection fails before authentication with:

```
    Failure establishing ssh session: -5, Unable to exchange encryption keys
```

Transfers run through curl's libssh2 backend, which reads `knownHosts` before the key exchange to pin the host key algorithm to the type recorded for the target host. libssh2 exposes no host name for a hashed entry, and curl treats such a nameless entry as a match without comparing host names, so it pins the key type of the first hashed entry in the file instead of the one belonging to the server ([`ssh_force_knownhost_key_type()`](https://github.com/curl/curl/blob/master/lib/vssh/libssh2.c), the `found = TRUE` branch taken when `store->name` is `NULL`). If that key type is one the server does not offer, the key exchange fails. Midnight Commander hit the same libssh2 limitation and describes it in detail in [MidnightCommander/mc#4506](https://github.com/MidnightCommander/mc/issues/4506); [curl#10143](https://github.com/curl/curl/issues/10143) shows the same error from the same function.

`ssh-keyscan` writes unhashed entries, so the command above is fine as it stands. What breaks it is pointing `knownHosts` at `~/.ssh/known_hosts`: `ssh` hashes what it adds, since `HashKnownHosts` is enabled by default on Debian and Ubuntu. That file is therefore usually hashed, which is why a plain `ssh` or `sftp` to the same server succeeds while the transfer here does not. Do not run `ssh-keygen -H` on the file given in `knownHosts`, and do not point the setting at a file that `ssh` maintains itself.

