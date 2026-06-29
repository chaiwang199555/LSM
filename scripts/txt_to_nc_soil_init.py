#!/usr/bin/env python3
"""
Convert LSM soil initial profile txt to NetCDF.

Text format (header lines starting with #):
    layer Tsoil theta

Usage:
    python scripts/txt_to_nc_soil_init.py data/txt/sample_soil_init.txt
    python scripts/txt_to_nc_soil_init.py data/txt/my_soil_init.txt -o data/nc/my_soil_init.nc
"""

from __future__ import annotations

import argparse
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
import xarray as xr

MISS = -9999.0

VAR_META = {
    "Tsoil": {"long_name": "soil temperature", "units": "K"},
    "theta": {"long_name": "volumetric soil moisture", "units": "m3 m-3"},
}


def read_soil_init_txt(path: Path) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    layers: list[int] = []
    tsoil: list[float] = []
    theta: list[float] = []

    with path.open() as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith("#"):
                continue
            parts = line.split()
            if len(parts) < 3:
                raise ValueError(f"Expected 3 columns, got {len(parts)}: {line[:80]}")
            layers.append(int(float(parts[0])))
            tsoil.append(float(parts[1]))
            theta.append(float(parts[2]))

    if not layers:
        raise ValueError(f"No data rows in {path}")

    layer_arr = np.array(layers, dtype=np.int32)
    tsoil_arr = np.array(tsoil, dtype=np.float64)
    theta_arr = np.array(theta, dtype=np.float64)

    mask_t = tsoil_arr <= MISS + 1.0
    mask_th = theta_arr <= MISS + 1.0
    tsoil_arr[mask_t] = 288.15
    theta_arr[mask_th] = 0.27

    return layer_arr, tsoil_arr, theta_arr


def txt_to_nc(txt_path: Path, nc_path: Path, site_name: str = "site") -> Path:
    txt_path = txt_path.resolve()
    nc_path = nc_path.resolve()
    nc_path.parent.mkdir(parents=True, exist_ok=True)

    layer, tsoil, theta = read_soil_init_txt(txt_path)
    nsoil = len(layer)

    ds = xr.Dataset(
        data_vars={
            "Tsoil": (["soil"], tsoil),
            "theta": (["soil"], theta),
        },
        coords={"soil": ("soil", layer)},
        attrs={
            "title": "LSM soil initial profile",
            "site_name": site_name,
            "nsoil": nsoil,
            "source_txt": str(txt_path),
            "created": datetime.now(timezone.utc).isoformat(),
            "Conventions": "CF-1.8",
            "note": "soil dimension index 1 = surface layer",
        },
    )

    for name, meta in VAR_META.items():
        ds[name].attrs.update(meta)
    ds["soil"].attrs["long_name"] = "soil layer index (1=surface)"

    encoding = {name: {"dtype": "float64", "_FillValue": MISS} for name in VAR_META}
    ds.to_netcdf(nc_path, encoding=encoding)
    print(f"Wrote {nsoil} layers -> {nc_path}")
    return nc_path


def main() -> None:
    parser = argparse.ArgumentParser(description="Convert LSM soil init txt to NetCDF")
    parser.add_argument("txt_file", type=Path, help="Input soil init txt file")
    parser.add_argument("-o", "--output", type=Path, default=None, help="Output .nc path")
    parser.add_argument("--site", type=str, default="site", help="Site name attribute")
    args = parser.parse_args()

    txt = args.txt_file
    out = Path("data/nc") / (txt.stem + ".nc") if args.output is None else args.output
    txt_to_nc(txt, out, site_name=args.site)


if __name__ == "__main__":
    main()