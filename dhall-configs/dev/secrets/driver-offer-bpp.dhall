let registrySecrets =
  { encryptionPrivKeyB16 = "801D25C612F7375EF24AFF0ABADEE2ECE9FEF4BD5E32481152B8523C1013B675"
    -- Private key PEM-encoded: MC4CAQAwBQYDK2VuBCIEIIAdJcYS9zde8kr/Crre4uzp/vS9XjJIEVK4UjwQE7Z1
    -- Associated public key in B64 format without ASN.1 headers: /JE0quNijg8AyxnRRNZBV3UpNyM8D47ta8LqU7N+a0o=
  }

in

{ dbUserId = "atlas_driver_offer_bpp_user"
, dbPassword = "atlas"
, smsOtpHash = "xxxxxxx"
, registrySecrets = registrySecrets
, signingKey = "ftjLZNZ6+QG8KAcNqax3NiX6Cg1bKVVdnbygReTwpFw="
, encHashSalt = "How wonderful it is that nobody need wait a single moment before starting to improve the world"
}