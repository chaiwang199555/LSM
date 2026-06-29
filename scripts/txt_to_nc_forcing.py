#!/usr/bin/env python3
"""
Convert LSM text forcing file to NetCDF.

Text format (header line starting with #):
    SW LW Ta P WS PA CO2 RH [cos_sza sw_beam_frac]

Usage:
    python scripts/txt_to_nc_forcing.py data/txt/sample_forcing.txt
    python scripts/txt_to_nc_forcing.py data/txt/my_forcing.txt -o data/nc/my_forcing.nc --dt 1800
"""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
import xarray as xr

MISS = -9999.0

VAR_META = {
    "SW":  {"long_name": "incoming shortwave radiation", "units": "W m-2"},
    "LW":  {"long_name": "incoming longwave radiation",  "units": "W m-2"},
    "Ta":  {"long_name": "air temperature",              "units": "K"},
    "P":   {"long_name": "precipitation",                "units": "mm"},
    "WS":  {"long_name": "wind speed",                   "units": "m s-1"},
    "PA":  {"long_name": "air pressure",                 "units": "Pa"},
    "CO2": {"long_name": "CO2 mole fraction",            "units": "ppm"},
    "RH":  {"long_name": "relative humidity",            "units": "%"},
    "cos_sza": {"long_name": "cosine of solar zenith angle", "units": "1"},
    "sw_beam_frac": {"long_name": "direct-beam fraction of shortwave", "units": "1"},
}

DEFAULTS = {
    "SW": 0.0,
    "LW": 300.0,
    "Ta": 288.15,
    "P": 0.0,
    "WS": 2.0,
    "PA": 101325.0,
    "CO2": 400.0,
    "RH": 70.0,
}


def read_forcing_txt(path: Path) -> dict[str, np.ndarray]:
    rows: list[list[float]] = []
    with path.open() as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            parts = line.split()
            if len(parts) < 8:
                raise ValueError(f"Expected at least 8 columns, got {len(parts)}: {line[:80]}")
            row = [float(x) for x in parts[:8]]
            if len(parts) >= 10:
                row.extend([float(parts[8]), float(parts[9])])
            else:
                row.extend([MISS, MISS])
            rows.append(row)

    if not rows:
        raise ValueError(f"No data rows in {path}")

    arr = np.array(rows, dtype=np.float64)
    cols = ["SW", "LW", "Ta", "P", "WS", "PA", "CO2", "RH", "cos_sza", "sw_beam_frac"]
    data = {name: arr[:, i].copy() for i, name in enumerate(cols)}

    for name, default in DEFAULTS.items():
        mask = data[name] <= MISS + 1.0
        data[name][mask] = default

    for name in ("cos_sza", "sw_beam_frac"):
        data[name][data[name] <= MISS + 1.0] = MISS

    return data


def txt_to_nc(
    txt_path: Path,
    nc_path: Path,
    dt: float = 1800.0,
    site_name: str = "site",
    t0: datetime | None = None,
) -> Path:
    txt_path = txt_path.resolve()
    nc_path = nc_path.resolve()
    nc_path.parent.mkdir(parents=True, exist_ok=True)

    data = read_forcing_txt(txt_path)
    ntime = len(data["SW"])

    times = np.arange(ntime, dtype=np.float64) * dt

    ds = xr.Dataset(
        data_vars={name: (["time"], data[name]) for name in VAR_META},
        coords={"time": ("time", times)},
        attrs={
            "title": "LSM atmospheric forcing",
            "site_name": site_name,
            "dt_seconds": dt,
            "source_txt": str(txt_path),
            "created": datetime.now(timezone.utc).isoformat(),
            "Conventions": "CF-1.8",
        },
    )

    for name, meta in VAR_META.items():
        ds[name].attrs.update(meta)
    ds["time"].attrs["long_name"] = "time since start"
    ds["time"].attrs["units"] = "s"
    ds["time"].attrs["axis"] = "T"

    encoding = {name: {"dtype": "float64", "_FillValue": MISS} for name in VAR_META}
    ds.to_netcdf(nc_path, encoding=encoding)
    print(f"Wrote {ntime} steps -> {nc_path}")
    return nc_path


def main() -> None:
    parser = argparse.ArgumentParser(description="Convert LSM txt forcing to NetCDF")
    parser.add_argument("txt_file", type=Path, help="Input txt forcing file")
    parser.add_argument("-o", "--output", type=Path, default=None, help="Output .nc path")
    parser.add_argument("--dt", type=float, default=1800.0, help="Time step (s)")
    parser.add_argument("--site", type=str, default="site", help="Site name attribute")
    args = parser.parse_args()

    txt = args.txt_file
    if args.output is None:
        out = Path("data/nc") / (txt.stem + ".nc")
    else:
        out = args.output

    txt_to_nc(txt, out, dt=args.dt, site_name=args.site)


if __name__ == "__main__":
    main()