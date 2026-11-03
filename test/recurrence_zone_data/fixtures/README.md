# Controlled timezone fixtures

These TZif bytes and reference observations come from the public-domain IANA
2025b release. They are immutable test inputs, not a bundled application timezone
database. Ordinary tests embed the committed bytes with `ppx_blob`; they never
read installed timezone rules, invoke tools, use a network, or infer the local
zone. `manifest.json` records each binary's exact SHA-256 and byte count.

Pinned sources:

- [tzcode2025b.tar.gz](https://data.iana.org/time-zones/releases/tzcode2025b.tar.gz),
  SHA-256 `05f8fedb3525ee70d49c87d3fae78a8a0dbae4fe87aa565c65cda9948ae135ec`.
- [tzdata2025b.tar.gz](https://data.iana.org/time-zones/releases/tzdata2025b.tar.gz),
  SHA-256 `11810413345fc7805017e27ea9fa4885fd74cd61b2911711ad038f5d28d71474`.

Both archives were unpacked into one isolated directory. Exact generation:

```sh
make zic zdump
./zic --version
./zdump --version
./zic -b slim -d zoneinfo northamerica australasia southamerica europe asia africa etcetera
./zdump -V -c 2050,2051 zoneinfo/America/New_York zoneinfo/Australia/Lord_Howe zoneinfo/America/Santiago zoneinfo/Europe/Dublin
./zdump -V -c 2100,2101 zoneinfo/Asia/Gaza
./zdump -V -c 2011,2012 zoneinfo/Pacific/Apia
./zdump -V -c 1999,2001 'STD0DST,J1/-167,J200/167' 'XXX3EDT4,0/0,J365/23' 'STD0DST,J1/-167,J365/167' 'STD-10:30DST-11,M10.1.0,M4.1.0'
./zdump -i -c 1999,2001 'STD0DST,J1/-167,J200/167' 'XXX3EDT4,0/0,J365/23' 'STD0DST,J1/-167,J365/167' 'STD-10:30DST-11,M10.1.0,M4.1.0'
./zdump -V -c 2399,2401 'STD0DST,J1/-167,J200/167'
./zdump -i -c 2000,2401 -t 5461633792,5461633793 'STD0DST,J1/-167,J200/167'
./zdump -i -c 2000,2401 -t 7161147007,7161147008 'STD0DST,J1/-167,J200/167'
```

Both version commands reported `2025b`. `make` used the reference Makefile's
unmodified defaults and system C compiler; no files were installed. Copy the
seven named binaries listed in `manifest.json`, replacing `/` by `_` and adding
`.tzif`. Captured output replaces only the absolute generation-directory prefix
with an empty string; offsets, instants and type labels are unchanged.

`footer-cycle-2000.txt` combines the verbose and brief 1999/2001 outputs. It
checks perpetual daylight, southern seasons and rules whose negative start time
crosses the cycle boundary. `footer-cycle-2400.txt` qualifies the opposite cycle
boundary beyond `Time_ns`. `footer-extremes.txt` records equivalent Gregorian
instants for raw signed 64-bit endpoints. Equivalents were computed using Python
unbounded integer arithmetic:

```python
equivalent = 946684800 + (raw_signed_seconds - 946684800) % 12622780800
```

The two bounded equivalent instants above were independently evaluated by the
pinned `zdump`; the production OCaml implementation must avoid overflowing the
subtraction at the original signed endpoints. Tests check the full final type
(offset, DST and designation), including rejecting each wrong component.

Future-2050 facts qualify the New York, Lord Howe, Santiago and Dublin footers.
The Gaza table extends past 2050, so 2100 qualifies its `/50` footer. Apia2011
qualifies an actual skipped civil day. The tests also use a separate integer
Gregorian interval oracle; it imports none of the production reader/calendar
helpers. These finite fixtures and bounded examples are evidence, not a proof
covering every future government rule change.

IANA's release `LICENSE` states that all files in the database are public domain
except explicitly identified exceptions. The source data files and the reference
`zic.c`, `zdump.c` and `localtime.c` used here carry public-domain notices. No
reference implementation code is copied into the production decoder. See the
[release LICENSE](https://data.iana.org/time-zones/tzdb-2025b/LICENSE) and
[reference source](https://data.iana.org/time-zones/tzdb-2025b/).
