namespace Testy3

open System.IO
type Hip =
    {
        HIP : int
        RArad : float
        DErad : float
        Hpmag: float32
    }
module LuiStars =
    type Marker = Marker
    let stars =
        let ass = typeof<Marker>.Assembly
        let name = ass.GetManifestResourceNames() |> Seq.find (fun n -> n.EndsWith "hip2_reduced.dat")
        use fs = ass.GetManifestResourceStream(name)
        use reader = new BinaryReader(fs)
        let cnt = int fs.Length / 24

        Array.init cnt (fun i ->
            {
                HIP  = reader.ReadInt32()
                RArad = reader.ReadDouble()
                DErad = reader.ReadDouble()
                Hpmag = reader.ReadSingle()
            }
        )
    let NamedStars : (string*int)[] = [| 
            ("Acamar",           13847);
            ("Groombridge 1830", 57939);
            ("Achernar",          7588);
            ("Hadar",            68702);
            ("Acrux",            60718);
            ("Hamal",             9884);
            ("Adhara",           33579);
            ("Izar",             72105);
            ("Agena",            68702);
            ("Kapteyn's star",   24186);
            ("Albireo",          95947);
            ("Kaus Australis",   90185);
            ("Alcor",            65477);
            ("Kocab",            72607);
            ("Alcyone",          17702);
            ("Kruger 60",       110893);
            ("Aldebaran",        21421);
            ("Luyten's star",    36208);
            ("Alderamin",       105199);
            ("Markab",          113963);
            ("Algenib",           1067);
            ("Megrez",           59774);
            ("Algieba",          50583);
            ("Menkar",           14135);
            ("Algol",            14576);
            ("Merak",            53910);
            ("Alhena",           31681);
            ("Mintaka",          25930);
            ("Alioth",           62956);
            ("Mira",             10826);
            ("Alkaid",           67301);
            ("Mirach",            5447);
            ("Almaak",            9640);
            ("Mirphak",          15863);
            ("Alnair",          109268);
            ("Mizar",            65378);
            ("Alnath",           25428);
            ("Nihalv",           25606);
            ("Alnilam",          26311);
            ("Nunki",            92855);
            ("Alnitak",          26727);
            ("Phad",             58001);
            ("Alphard",          46390);
            ("Pleione",          17851);
            ("Alphekka",         76267);
            ("Polaris",          11767);
            ("Alpheratz",          677);
            ("Pollux",           37826);
            ("Alshain",          98036);
            ("Procyon",          37279);
            ("Altair",           97649);
            ("Proxima",          70890);
            ("Ankaa",             2081);
            ("Rasalgethi",       84345);
            ("Antares",          80763);
            ("Rasalhaguev",      86032);
            ("Arcturus",         69673);
            ("Red Rectangle",    30089);
            ("Arneb",            25985);
            ("Regulus",          49669);
            ("Babcock's star",  112247);
            ("Rigel",            24436);
            ("Barnard's star",   87937);
            ("Rigil Kent",       71683);
            ("Bellatrix",        25336);
            ("Sadalmelik",      109074);
            ("Betelgeuse",       27989);
            ("Saiph",            27366);
            ("Campbell's star",  96295);
            ("Scheat",          113881);
            ("Canopus",          30438);
            ("Shaula",           85927);
            ("Capella",          24608);
            ("Shedir",            3179);
            ("Caph",               746);
            ("Sheliak",          92420);
            ("Castor",           36850);
            ("Sirius",           32349);
            ("Cor Caroli",       63125);
            ("Spica",            65474);
            ("Cyg X-1",          98298);
            ("Tarazed",          97278);
            ("Deneb",           102098);
            ("Thuban",           68756);
            ("Denebola",         57632);
            ("Unukalhai",        77070);
            ("Diphda",            3419);
            ("Van Maanen 2",      3829);
            ("Dubhe",            54061);
            ("Vega",             91262);
            ("Enif",            107315);
            ("Vindemiatrix",     63608);
            ("Etamin",           87833);
            ("Zaurak",           18543);
            ("Fomalhaut",       113368);
            ("3C 273",           60936);
        |]
    let ursaMajorStars : (string*int)[] = [| // the wagon
        ("Megrez",           59774); // mag 3.312
        ("Merak",            53910);
        ("Alioth",           62956);
        ("Alkaid",           67301);
        ("Mizar",            65378);
        ("Phekta",           58001); // Phad
        ("Dubhe",            54061);

        ("Polaris",          11767); // not part of ursaMajor, just for orientation
        ("Vega",             91262); // not part of ursaMajor, just for orientation (mag 0)
    |]
    let ursaMajor = [|
        (67301, 65378) // Alkaid - Mizar
        (65378, 62956) // Mizar - Alioth
        (62956, 59774) // Alioth - Megrez
        (59774, 58001) // Megrez - Phekta
        (58001, 53910) // Phekta - Merak
        (53910, 54061) // Merak - Dubhe
        (54061, 59774) // Dubhe - Megrez
    |]

    let ursaMinor = [| // Wagon of Heaven
        (11767, 85822) // Alpha UMi / Polaris - Delta UMi / Yildun / Pherkard
        (85822, 82080) // Delta UMi / Yildun / Pherkard - Epsilon UMi
        (82080, 77055) // Epsilon UMi - zeta?
        (77055, 79822) // zeta - n / Alasco
        (79822, 75097) // n / Alasco - Gamma Umi / Pherkad 
        (75097, 72607) // Gamma Umi / Pherkad - Beta UMi / Kochab / Kocab
        (72607, 77055) // Beta UMi / Kochab / Kocab - zeta
    |]

    let orion = [| 
        (27366, 26727) // Saiph - Alnitak
        (26727, 27989) // Alnitak - Betelgeuse
        (27989, 26207) // Betelgeuse - Meissa
        (26207, 25336) // Meissa - Bellatrix
        (25336, 25930) // Bellatrix - Mintaka
        (25930, 26311) // Mintaka - Alnilam
        (26311, 26727) // Alnilam - Alnitak
        (25930, 24436) // Mintaka - Rigel
    |]

    let cassiopeia = [|
        (8886, 6686) // Epsilon Cas - Delta Cas
        (6686, 4427) // Delta Cas - Gamma Cas
        (4427, 3179) // Gamma Cas - Alpha Cas
        (3179, 746) // Alpha Cas - Beta Cas / Caph
    |]

    let virgo = [|
        (65474, 64238) // Porrima - X
        (64238, 61941) // X - Porrima
        (61941, 63090) // Porrima - Auva
        (63090, 63608) // Auva, Vindemiatrix
        (61941, 60129) // Porrima - Zaniah
        (60129, 57757) // Zaniah - Zavijava
        (64238, 66249) // X - Heze
    |]

    let andromeda = [|
        (9640, 5447) // Almach - Mirach
        (5447, 3092) // Mirach - spectroscopic binary
        (3092, 677)  // spectroscopic binary - Alpheratz
        (5447, 4436) // Mirach - multi star
        (4436, 3881) //	multi star - spectroscopic binary
    |]

    let all = [|
        ursaMajor
        ursaMinor
        orion
        cassiopeia
        virgo
        andromeda
    |]  