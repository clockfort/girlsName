module CensusData where
 
--Source: U.S. Census Bureau, 1990 Census
girlsNames = [
    "MARY",
    "PATRICIA",
    "LINDA",
    "BARBARA",
    "ELIZABETH",
    "JENNIFER",
    "MARIA",
    "SUSAN",
    "MARGARET",
    "DOROTHY",
    "LISA",
    "NANCY",
    "KAREN",
    "BETTY",
    "HELEN",
    "SANDRA",
    "DONNA",
    "CAROL",
    "RUTH",
    "SHARON",
    "MICHELLE",
    "LAURA",
    "SARAH",
    "KIMBERLY",
    "DEBORAH",
    "JESSICA",
    "SHIRLEY",
    "CYNTHIA",
    "ANGELA",
    "MELISSA",
    "BRENDA",
    "AMY",
    "ANNA",
    "REBECCA",
    "VIRGINIA",
    "KATHLEEN",
    "PAMELA",
    "MARTHA",
    "DEBRA",
    "AMANDA",
    "STEPHANIE",
    "CAROLYN",
    "CHRISTINE",
    "MARIE",
    "JANET",
    "CATHERINE",
    "FRANCES",
    "ANN",
    "JOYCE",
    "DIANE",
    "ALICE",
    "JULIE",
    "HEATHER",
    "TERESA",
    "DORIS",
    "GLORIA",
    "EVELYN",
    "JEAN",
    "CHERYL",
    "MILDRED",
    "KATHERINE",
    "JOAN",
    "ASHLEY",
    "JUDITH",
    "ROSE",
    "JANICE",
    "KELLY",
    "NICOLE",
    "JUDY",
    "CHRISTINA",
    "KATHY",
    "THERESA",
    "BEVERLY",
    "DENISE",
    "TAMMY",
    "IRENE",
    "JANE",
    "LORI",
    "RACHEL",
    "MARILYN",
    "ANDREA",
    "KATHRYN",
    "LOUISE",
    "SARA",
    "ANNE",
    "JACQUELINE",
    "WANDA",
    "BONNIE",
    "JULIA",
    "RUBY",
    "LOIS",
    "TINA",
    "PHYLLIS",
    "NORMA",
    "PAULA",
    "DIANA",
    "ANNIE",
    "LILLIAN",
    "EMILY",
    "ROBIN",
    "PEGGY",
    "CRYSTAL",
    "GLADYS",
    "RITA",
    "DAWN",
    "CONNIE",
    "FLORENCE",
    "TRACY",
    "EDNA",
    "TIFFANY",
    "CARMEN",
    "ROSA",
    "CINDY",
    "GRACE",
    "WENDY",
    "VICTORIA",
    "EDITH",
    "KIM",
    "SHERRY",
    "SYLVIA",
    "JOSEPHINE",
    "THELMA",
    "SHANNON",
    "SHEILA",
    "ETHEL",
    "ELLEN",
    "ELAINE",
    "MARJORIE",
    "CARRIE",
    "CHARLOTTE",
    "MONICA",
    "ESTHER",
    "PAULINE",
    "EMMA",
    "JUANITA",
    "ANITA",
    "RHONDA",
    "HAZEL",
    "AMBER",
    "EVA",
    "DEBBIE",
    "APRIL",
    "LESLIE",
    "CLARA",
    "LUCILLE",
    "JAMIE",
    "JOANNE",
    "ELEANOR",
    "VALERIE",
    "DANIELLE",
    "MEGAN",
    "ALICIA",
    "SUZANNE",
    "MICHELE",
    "GAIL",
    "BERTHA",
    "DARLENE",
    "VERONICA",
    "JILL",
    "ERIN",
    "GERALDINE",
    "LAUREN",
    "CATHY",
    "JOANN",
    "LORRAINE",
    "LYNN",
    "SALLY",
    "REGINA",
    "ERICA",
    "BEATRICE",
    "DOLORES",
    "BERNICE",
    "AUDREY",
    "YVONNE",
    "ANNETTE",
    "JUNE",
    "SAMANTHA",
    "MARION",
    "DANA",
    "STACY",
    "ANA",
    "RENEE",
    "IDA",
    "VIVIAN",
    "ROBERTA",
    "HOLLY",
    "BRITTANY",
    "MELANIE",
    "LORETTA",
    "YOLANDA",
    "JEANETTE",
    "LAURIE",
    "KATIE",
    "KRISTEN",
    "VANESSA",
    "ALMA",
    "SUE",
    "ELSIE",
    "BETH",
    "JEANNE",
    "VICKI",
    "CARLA",
    "TARA",
    "ROSEMARY",
    "EILEEN",
    "TERRI",
    "GERTRUDE",
    "LUCY",
    "TONYA",
    "ELLA",
    "STACEY",
    "WILMA",
    "GINA",
    "KRISTIN",
    "JESSIE",
    "NATALIE",
    "AGNES",
    "VERA",
    "WILLIE",
    "CHARLENE",
    "BESSIE",
    "DELORES",
    "MELINDA",
    "PEARL",
    "ARLENE",
    "MAUREEN",
    "COLLEEN",
    "ALLISON",
    "TAMARA",
    "JOY",
    "GEORGIA",
    "CONSTANCE",
    "LILLIE",
    "CLAUDIA",
    "JACKIE",
    "MARCIA",
    "TANYA",
    "NELLIE",
    "MINNIE",
    "MARLENE",
    "HEIDI",
    "GLENDA",
    "LYDIA",
    "VIOLA",
    "COURTNEY",
    "MARIAN",
    "STELLA",
    "CAROLINE",
    "DORA",
    "JO",
    "VICKIE",
    "MATTIE",
    "TERRY",
    "MAXINE",
    "IRMA",
    "MABEL",
    "MARSHA",
    "MYRTLE",
    "LENA",
    "CHRISTY",
    "DEANNA",
    "PATSY",
    "HILDA",
    "GWENDOLYN",
    "JENNIE",
    "NORA",
    "MARGIE",
    "NINA",
    "CASSANDRA",
    "LEAH",
    "PENNY",
    "KAY",
    "PRISCILLA",
    "NAOMI",
    "CAROLE",
    "BRANDY",
    "OLGA",
    "BILLIE",
    "DIANNE",
    "TRACEY",
    "LEONA",
    "JENNY",
    "FELICIA",
    "SONIA",
    "MIRIAM",
    "VELMA",
    "BECKY",
    "BOBBIE",
    "VIOLET",
    "KRISTINA",
    "TONI",
    "MISTY",
    "MAE",
    "SHELLY",
    "DAISY",
    "RAMONA",
    "SHERRI",
    "ERIKA",
    "KATRINA",
    "CLAIRE",
    "LINDSEY",
    "LINDSAY",
    "GENEVA",
    "GUADALUPE",
    "BELINDA",
    "MARGARITA",
    "SHERYL",
    "CORA",
    "FAYE",
    "ADA",
    "NATASHA",
    "SABRINA",
    "ISABEL",
    "MARGUERITE",
    "HATTIE",
    "HARRIET",
    "MOLLY",
    "CECILIA",
    "KRISTI",
    "BRANDI",
    "BLANCHE",
    "SANDY",
    "ROSIE",
    "JOANNA",
    "IRIS",
    "EUNICE",
    "ANGIE",
    "INEZ",
    "LYNDA",
    "MADELINE",
    "AMELIA",
    "ALBERTA",
    "GENEVIEVE",
    "MONIQUE",
    "JODI",
    "JANIE",
    "MAGGIE",
    "KAYLA",
    "SONYA",
    "JAN",
    "LEE",
    "KRISTINE",
    "CANDACE",
    "FANNIE",
    "MARYANN",
    "OPAL",
    "ALISON",
    "YVETTE",
    "MELODY",
    "LUZ",
    "SUSIE",
    "OLIVIA",
    "FLORA",
    "SHELLEY",
    "KRISTY",
    "MAMIE",
    "LULA",
    "LOLA",
    "VERNA",
    "BEULAH",
    "ANTOINETTE",
    "CANDICE",
    "JUANA",
    "JEANNETTE",
    "PAM",
    "KELLI",
    "HANNAH",
    "WHITNEY",
    "BRIDGET",
    "KARLA",
    "CELIA",
    "LATOYA",
    "PATTY",
    "SHELIA",
    "GAYLE",
    "DELLA",
    "VICKY",
    "LYNNE",
    "SHERI",
    "MARIANNE",
    "KARA",
    "JACQUELYN",
    "ERMA",
    "BLANCA",
    "MYRA",
    "LETICIA",
    "PAT",
    "KRISTA",
    "ROXANNE",
    "ANGELICA",
    "JOHNNIE",
    "ROBYN",
    "FRANCIS",
    "ADRIENNE",
    "ROSALIE",
    "ALEXANDRA",
    "BROOKE",
    "BETHANY",
    "SADIE",
    "BERNADETTE",
    "TRACI",
    "JODY",
    "KENDRA",
    "JASMINE",
    "NICHOLE",
    "RACHAEL",
    "CHELSEA",
    "MABLE",
    "ERNESTINE",
    "MURIEL",
    "MARCELLA",
    "ELENA",
    "KRYSTAL",
    "ANGELINA",
    "NADINE",
    "KARI",
    "ESTELLE",
    "DIANNA",
    "PAULETTE",
    "LORA",
    "MONA",
    "DOREEN",
    "ROSEMARIE",
    "ANGEL",
    "DESIREE",
    "ANTONIA",
    "HOPE",
    "GINGER",
    "JANIS",
    "BETSY",
    "CHRISTIE",
    "FREDA",
    "MERCEDES",
    "MEREDITH",
    "LYNETTE",
    "TERI",
    "CRISTINA",
    "EULA",
    "LEIGH",
    "MEGHAN",
    "SOPHIA",
    "ELOISE",
    "ROCHELLE",
    "GRETCHEN",
    "CECELIA",
    "RAQUEL",
    "HENRIETTA",
    "ALYSSA",
    "JANA",
    "KELLEY",
    "GWEN",
    "KERRY",
    "JENNA",
    "TRICIA",
    "LAVERNE",
    "OLIVE",
    "ALEXIS",
    "TASHA",
    "SILVIA",
    "ELVIRA",
    "CASEY",
    "DELIA",
    "SOPHIE",
    "KATE",
    "PATTI",
    "LORENA",
    "KELLIE",
    "SONJA",
    "LILA",
    "LANA",
    "DARLA",
    "MAY",
    "MINDY",
    "ESSIE",
    "MANDY",
    "LORENE",
    "ELSA",
    "JOSEFINA",
    "JEANNIE",
    "MIRANDA",
    "DIXIE",
    "LUCIA",
    "MARTA",
    "FAITH",
    "LELA",
    "JOHANNA",
    "SHARI",
    "CAMILLE",
    "TAMI",
    "SHAWNA",
    "ELISA",
    "EBONY",
    "MELBA",
    "ORA",
    "NETTIE",
    "TABITHA",
    "OLLIE",
    "JAIME",
    "WINIFRED",
    "KRISTIE",
    "MARINA",
    "ALISHA",
    "AIMEE",
    "RENA",
    "MYRNA",
    "MARLA",
    "TAMMIE",
    "LATASHA",
    "BONITA",
    "PATRICE",
    "RONDA",
    "SHERRIE",
    "ADDIE",
    "FRANCINE",
    "DELORIS",
    "STACIE",
    "ADRIANA",
    "CHERI",
    "SHELBY",
    "ABIGAIL",
    "CELESTE",
    "JEWEL",
    "CARA",
    "ADELE",
    "REBEKAH",
    "LUCINDA",
    "DORTHY",
    "CHRIS",
    "EFFIE",
    "TRINA",
    "REBA",
    "SHAWN",
    "SALLIE",
    "AURORA",
    "LENORA",
    "ETTA",
    "LOTTIE",
    "KERRI",
    "TRISHA",
    "NIKKI",
    "ESTELLA",
    "FRANCISCA",
    "JOSIE",
    "TRACIE",
    "MARISSA",
    "KARIN",
    "BRITTNEY",
    "JANELLE",
    "LOURDES",
    "LAUREL",
    "HELENE",
    "FERN",
    "ELVA",
    "CORINNE",
    "KELSEY",
    "INA",
    "BETTIE",
    "ELISABETH",
    "AIDA",
    "CAITLIN",
    "INGRID",
    "IVA",
    "EUGENIA",
    "CHRISTA",
    "GOLDIE",
    "CASSIE",
    "MAUDE",
    "JENIFER",
    "THERESE",
    "FRANKIE",
    "DENA",
    "LORNA",
    "JANETTE",
    "LATONYA",
    "CANDY",
    "MORGAN",
    "CONSUELO",
    "TAMIKA",
    "ROSETTA",
    "DEBORA",
    "CHERIE",
    "POLLY",
    "DINA",
    "JEWELL",
    "FAY",
    "JILLIAN",
    "DOROTHEA",
    "NELL",
    "TRUDY",
    "ESPERANZA",
    "PATRICA",
    "KIMBERLEY",
    "SHANNA",
    "HELENA",
    "CAROLINA",
    "CLEO",
    "STEFANIE",
    "ROSARIO",
    "OLA",
    "JANINE",
    "MOLLIE",
    "LUPE",
    "ALISA",
    "LOU",
    "MARIBEL",
    "SUSANNE",
    "BETTE",
    "SUSANA",
    "ELISE",
    "CECILE",
    "ISABELLE",
    "LESLEY",
    "JOCELYN",
    "PAIGE",
    "JONI",
    "RACHELLE",
    "LEOLA",
    "DAPHNE",
    "ALTA",
    "ESTER",
    "PETRA",
    "GRACIELA",
    "IMOGENE",
    "JOLENE",
    "KEISHA",
    "LACEY",
    "GLENNA",
    "GABRIELA",
    "KERI",
    "URSULA",
    "LIZZIE",
    "KIRSTEN",
    "SHANA",
    "ADELINE",
    "MAYRA",
    "JAYNE",
    "JACLYN",
    "GRACIE",
    "SONDRA",
    "CARMELA",
    "MARISA",
    "ROSALIND",
    "CHARITY",
    "TONIA",
    "BEATRIZ",
    "MARISOL",
    "CLARICE",
    "JEANINE",
    "SHEENA",
    "ANGELINE",
    "FRIEDA",
    "LILY",
    "ROBBIE",
    "SHAUNA",
    "MILLIE",
    "CLAUDETTE",
    "CATHLEEN",
    "ANGELIA",
    "GABRIELLE",
    "AUTUMN",
    "KATHARINE",
    "SUMMER",
    "JODIE",
    "STACI",
    "LEA",
    "CHRISTI",
    "JIMMIE",
    "JUSTINE",
    "ELMA",
    "LUELLA",
    "MARGRET",
    "DOMINIQUE",
    "SOCORRO",
    "RENE",
    "MARTINA",
    "MARGO",
    "MAVIS",
    "CALLIE",
    "BOBBI",
    "MARITZA",
    "LUCILE",
    "LEANNE",
    "JEANNINE",
    "DEANA",
    "AILEEN",
    "LORIE",
    "LADONNA",
    "WILLA",
    "MANUELA",
    "GALE",
    "SELMA",
    "DOLLY",
    "SYBIL",
    "ABBY",
    "LARA",
    "DALE",
    "IVY",
    "DEE",
    "WINNIE",
    "MARCY",
    "LUISA",
    "JERI",
    "MAGDALENA",
    "OFELIA",
    "MEAGAN",
    "AUDRA",
    "MATILDA",
    "LEILA",
    "CORNELIA",
    "BIANCA",
    "SIMONE",
    "BETTYE",
    "RANDI",
    "VIRGIE",
    "LATISHA",
    "BARBRA",
    "GEORGINA",
    "ELIZA",
    "LEANN",
    "BRIDGETTE",
    "RHODA",
    "HALEY",
    "ADELA",
    "NOLA",
    "BERNADINE",
    "FLOSSIE",
    "ILA",
    "GRETA",
    "RUTHIE",
    "NELDA",
    "MINERVA",
    "LILLY",
    "TERRIE",
    "LETHA",
    "HILARY",
    "ESTELA",
    "VALARIE",
    "BRIANNA",
    "ROSALYN",
    "EARLINE",
    "CATALINA",
    "AVA",
    "MIA",
    "CLARISSA",
    "LIDIA",
    "CORRINE",
    "ALEXANDRIA",
    "CONCEPCION",
    "TIA",
    "SHARRON",
    "RAE",
    "DONA",
    "ERICKA",
    "JAMI",
    "ELNORA",
    "CHANDRA",
    "LENORE",
    "NEVA",
    "MARYLOU",
    "MELISA",
    "TABATHA",
    "SERENA",
    "AVIS",
    "ALLIE",
    "SOFIA",
    "JEANIE",
    "ODESSA",
    "NANNIE",
    "HARRIETT",
    "LORAINE",
    "PENELOPE",
    "MILAGROS",
    "EMILIA",
    "BENITA",
    "ALLYSON",
    "ASHLEE",
    "TANIA",
    "TOMMIE",
    "ESMERALDA",
    "KARINA",
    "EVE",
    "PEARLIE",
    "ZELMA",
    "MALINDA",
    "NOREEN",
    "TAMEKA",
    "SAUNDRA",
    "HILLARY",
    "AMIE",
    "ALTHEA",
    "ROSALINDA",
    "JORDAN",
    "LILIA",
    "ALANA",
    "GAY",
    "CLARE",
    "ALEJANDRA",
    "ELINOR",
    "MICHAEL",
    "LORRIE",
    "JERRI",
    "DARCY",
    "EARNESTINE",
    "CARMELLA",
    "TAYLOR",
    "NOEMI",
    "MARCIE",
    "LIZA",
    "ANNABELLE",
    "LOUISA",
    "EARLENE",
    "MALLORY",
    "CARLENE",
    "NITA",
    "SELENA",
    "TANISHA",
    "KATY",
    "JULIANNE",
    "JOHN",
    "LAKISHA",
    "EDWINA",
    "MARICELA",
    "MARGERY",
    "KENYA",
    "DOLLIE",
    "ROXIE",
    "ROSLYN",
    "KATHRINE",
    "NANETTE",
    "CHARMAINE",
    "LAVONNE",
    "ILENE",
    "KRIS",
    "TAMMI",
    "SUZETTE",
    "CORINE",
    "KAYE",
    "JERRY",
    "MERLE",
    "CHRYSTAL",
    "LINA",
    "DEANNE",
    "LILIAN",
    "JULIANA",
    "ALINE",
    "LUANN",
    "KASEY",
    "MARYANNE",
    "EVANGELINE",
    "COLETTE",
    "MELVA",
    "LAWANDA",
    "YESENIA",
    "NADIA",
    "MADGE",
    "KATHIE",
    "EDDIE",
    "OPHELIA",
    "VALERIA",
    "NONA",
    "MITZI",
    "MARI",
    "GEORGETTE",
    "CLAUDINE",
    "FRAN",
    "ALISSA",
    "ROSEANN",
    "LAKEISHA",
    "SUSANNA",
    "REVA",
    "DEIDRE",
    "CHASITY",
    "SHEREE",
    "CARLY",
    "JAMES",
    "ELVIA",
    "ALYCE",
    "DEIRDRE",
    "GENA",
    "BRIANA",
    "ARACELI",
    "KATELYN",
    "ROSANNE",
    "WENDI",
    "TESSA",
    "BERTA",
    "MARVA",
    "IMELDA",
    "MARIETTA",
    "MARCI",
    "LEONOR",
    "ARLINE",
    "SASHA",
    "MADELYN",
    "JANNA",
    "JULIETTE",
    "DEENA",
    "AURELIA",
    "JOSEFA",
    "AUGUSTA",
    "LILIANA",
    "YOUNG",
    "CHRISTIAN",
    "LESSIE",
    "AMALIA",
    "SAVANNAH",
    "ANASTASIA",
    "VILMA",
    "NATALIA",
    "ROSELLA",
    "LYNNETTE",
    "CORINA",
    "ALFREDA",
    "LEANNA",
    "CAREY",
    "AMPARO",
    "COLEEN",
    "TAMRA",
    "AISHA",
    "WILDA",
    "KARYN",
    "CHERRY",
    "QUEEN",
    "MAURA",
    "MAI",
    "EVANGELINA",
    "ROSANNA",
    "HALLIE",
    "ERNA",
    "ENID",
    "MARIANA",
    "LACY",
    "JULIET",
    "JACKLYN",
    "FREIDA",
    "MADELEINE",
    "MARA",
    "HESTER",
    "CATHRYN",
    "LELIA",
    "CASANDRA",
    "BRIDGETT",
    "ANGELITA",
    "JANNIE",
    "DIONNE",
    "ANNMARIE",
    "KATINA",
    "BERYL",
    "PHOEBE",
    "MILLICENT",
    "KATHERYN",
    "DIANN",
    "CARISSA",
    "MARYELLEN",
    "LIZ",
    "LAURI",
    "HELGA",
    "GILDA",
    "ADRIAN",
    "RHEA",
    "MARQUITA",
    "HOLLIE",
    "TISHA",
    "TAMERA",
    "ANGELIQUE",
    "FRANCESCA",
    "BRITNEY",
    "KAITLIN",
    "LOLITA",
    "FLORINE",
    "ROWENA",
    "REYNA",
    "TWILA",
    "FANNY",
    "JANELL",
    "INES",
    "CONCETTA",
    "BERTIE",
    "ALBA",
    "BRIGITTE",
    "ALYSON",
    "VONDA",
    "PANSY",
    "ELBA",
    "NOELLE",
    "LETITIA",
    "KITTY",
    "DEANN",
    "BRANDIE",
    "LOUELLA",
    "LETA",
    "FELECIA",
    "SHARLENE",
    "LESA",
    "BEVERLEY",
    "ROBERT",
    "ISABELLA",
    "HERMINIA",
    "TERRA",
    "CELINA"
    ]