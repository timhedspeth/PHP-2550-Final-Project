library(data.table)

sal_usa <- read.csv("updated_salmonella_US.csv")
dim(sal_usa) #297222     21
length(unique(sal_usa$Isolation.source)) #3779

sal_usa$Isolation.source.category <- sal_usa$Isolation.source

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "milk" == TRUE] <- "Milk and related products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Milk" == TRUE] <- "Milk and related products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "ice cream" == TRUE] <- "Milk and related products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "cream" == TRUE] <- "Milk and related products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Cream" == TRUE] <- "Milk and related products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Cheese" == TRUE] <- "Milk and related products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "cheese" == TRUE] <- "Milk and related products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "whey" == TRUE] <- "Milk and related products"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Egg" == TRUE] <- "Eggs and related products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "egg" == TRUE] <- "Eggs and related products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "EGG" == TRUE] <- "Eggs and related products"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "chick feed" == TRUE] <- "Poultry feed"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "feed imported" == TRUE] <- "Poultry feed"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Gallus gallus" == TRUE] <- "Chicken and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "gallus gallus" == TRUE] <- "Chicken and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Chicken" == TRUE] <- "Chicken and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "chicken" == TRUE] <- "Chicken and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "CHICKEN" == TRUE] <- "Chicken and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Gallus gallus domesticus" == TRUE] <- "Chicken and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "wing" == TRUE] <- "Chicken and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Wing" == TRUE] <- "Chicken and related"




sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Turkey" == TRUE] <- "Turkey and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "turkey" == TRUE] <- "Turkey and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Meleagris gallopav" == TRUE] <- "Turkey and related"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Pork" == TRUE] <- "Pork and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "pork" == TRUE] <- "Pork and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "sausage" == TRUE] <- "Pork and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Sausage" == TRUE] <- "Pork and related"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "salami" == TRUE] <- "Pork and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Salami" == TRUE] <- "Pork and related"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "pig" == TRUE] <- "Pig and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Pig" == TRUE] <- "Pig and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Swine" == TRUE] <- "Pig and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "swine" == TRUE] <- "Pig and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "porcine" == TRUE] <- "Pig and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Porcine" == TRUE] <- "Pig and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Sus scrofa" == TRUE] <- "Pig and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Sus domesticus" == TRUE] <- "Pig and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Hog" == TRUE] <- "Pig and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "hog" == TRUE] <- "Pig and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Sus scrofa domesticus" == TRUE] <- "Pig and related"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "beef" == TRUE] <- "Beef and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Beef" == TRUE] <- "Beef and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "BEEF" == TRUE] <- "Beef and related"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "cow" == TRUE] <- "Cattle, cow and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Bos taurus" == TRUE] <- "Cattle, cow and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "cattle" == TRUE] <- "Cattle, cow and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Cattle" == TRUE] <- "Cattle, cow and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "bovine" == TRUE] <- "Cattle, cow and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Bovine" == TRUE] <- "Cattle, cow and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "calf" == TRUE] <- "Cattle, cow and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Calf" == TRUE] <- "Cattle, cow and related"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Lamb" == TRUE] <- "Lamb and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "lamb" == TRUE] <- "Lamb and related"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "ovine" == TRUE] <- "Sheep, goat and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Ovine" == TRUE] <- "Sheep, goat and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Goat" == TRUE] <- "Sheep, goat and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "goat" == TRUE] <- "Sheep, goat and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Sheep" == TRUE] <- "Sheep, goat and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "sheep" == TRUE] <- "Sheep, goat and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Ovis canadensis)" == TRUE] <- "Sheep, goat and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Ovis aries" == TRUE] <- "Sheep, goat and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "ovis aries" == TRUE] <- "Sheep, goat and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "ovis" == TRUE] <- "Sheep, goat and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Ovis" == TRUE] <- "Sheep, goat and related"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "meat" == TRUE] <- "Unspecified meat and meat products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Meat" == TRUE] <- "Unspecified meat and meat products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "barbeque" == TRUE] <- "Unspecified meat and meat products"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "duck" == TRUE] <- "Duck and goose"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Duck" == TRUE] <- "Duck and goose"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "goose" == TRUE] <- "Duck and goose"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Goose" == TRUE] <- "Duck and goose"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Poultry" == TRUE] <- "Meat and meat products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "poultry" == TRUE] <- "Meat and meat products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "avian" == TRUE] <- "Meat and meat products"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "rabbit" == TRUE] <- "Other animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Rabbit" == TRUE] <- "Other animals and related"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Felis catus" == TRUE] <- "Cat, dog and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Canis lupus familiaris" == TRUE] <- "Cat, dog and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "dog food" == TRUE] <- "Cat, dog and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "cat food" == TRUE] <- "Cat, dog and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "dog" == TRUE] <- "Cat, dog and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Dog" == TRUE] <- "Cat, dog and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "cat" == TRUE] <- "Cat, dog and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Cat" == TRUE] <- "Cat, dog and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "feline" == TRUE] <- "Cat, dog and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Feline" == TRUE] <- "Cat, dog and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "canine" == TRUE] <- "Cat, dog and related"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "pet" == TRUE] <- "Pet and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Pet" == TRUE] <- "Pet and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Pet" == TRUE] <- "Pet and related"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "horse" == TRUE] <- "Horses, alpaca, deer and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Horse" == TRUE] <- "Horses, alpaca, deer and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Lama glama" == TRUE] <- "Horses, alpaca, deer and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "kangaroo" == TRUE] <- "Horses, alpaca, deer and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "camel" == TRUE] <- "Horses, alpaca, deer and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "llama" == TRUE] <- "Horses, alpaca, deer and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "alpaca" == TRUE] <- "Horses, alpaca, deer and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "equus caballus" == TRUE] <- "Horses, alpaca, deer and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Equus caballus" == TRUE] <- "Horses, alpaca, deer and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "equine" == TRUE] <- "Horses, alpaca, deer and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Equine" == TRUE] <- "Horses, alpaca, deer and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "moose" == TRUE] <- "Horses, alpaca, deer and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Moose" == TRUE] <- "Horses, alpaca, deer and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "elk" == TRUE] <- "Horses, alpaca, deer and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "deer" == TRUE] <- "Horses, alpaca, deer and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Deer" == TRUE] <- "Horses, alpaca, deer and similar"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "raccoon" == TRUE] <- "Bear, raccoon and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "brown" == TRUE] <- "Bear, raccoon and similar"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Brown" == TRUE] <- "Bear, raccoon and similar"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Monkey" == TRUE] <- "Monkey and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "monkey" == TRUE] <- "Monkey and related"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "animal" == TRUE] <- "Other animials: unspecified"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "mouse" == TRUE] <- "Mouse, hamster and others"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Mouse" == TRUE] <- "Mouse, hamster and others"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "hamster" == TRUE] <- "Mouse, hamster and others"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Hamster" == TRUE] <- "Mouse, hamster and others"




sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "fish" == TRUE] <- "Seafood and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "SHRIMP" == TRUE] <- "Seafood and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "shrimp" == TRUE] <- "Seafood and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Shrimp" == TRUE] <- "Seafood and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "SALMON" == TRUE] <- "Seafood and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "salmon" == TRUE] <- "Seafood and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Salmon" == TRUE] <- "Seafood and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "clam" == TRUE] <- "Seafood and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Jambalaya" == TRUE] <- "Seafood and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "tuna" == TRUE] <- "Seafood and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Tuna" == TRUE] <- "Seafood and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "TUNA" == TRUE] <- "Seafood and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "crab" == TRUE] <- "Seafood and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Crab" == TRUE] <- "Seafood and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "CRAB" == TRUE] <- "Seafood and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "oyster" == TRUE] <- "Seafood and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "fillet" == TRUE] <- "Seafood and related"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "turtle" == TRUE] <- "Aquatic animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Turtle" == TRUE] <- "Aquatic animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "sea lion" == TRUE] <- "Aquatic animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "tortoise" == TRUE] <- "Aquatic animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "tortoise" == TRUE] <- "Aquatic animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Neovison vison" == TRUE] <- "Aquatic animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Enhydra" == TRUE] <- "Aquatic animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "marine mammal" == TRUE] <- "Aquatic animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Siluriformes" == TRUE] <- "Aquatic animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "siluriformes" == TRUE] <- "Aquatic animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Hip" == TRUE] <- "Aquatic animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "hip" == TRUE] <- "Aquatic animals and related"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "bird" == TRUE] <- "Flying animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Bird" == TRUE] <- "Flying animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Pigeon" == TRUE] <- "Flying animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "pigeon" == TRUE] <- "Flying animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "quail" == TRUE] <- "Flying animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Quail" == TRUE] <- "Flying animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Seagull" == TRUE] <- "Flying animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "seagull" == TRUE] <- "Flying animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "parrot" == TRUE] <- "Flying animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Parrot" == TRUE] <- "Flying animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "sparrow" == TRUE] <- "Flying animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Eudocimus albus" == TRUE] <- "Flying animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "owl" == TRUE] <- "Flying animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Owl" == TRUE] <- "Flying animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "bat" == TRUE] <- "Flying animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Bat" == TRUE] <- "Flying animals and products"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Pogona" == TRUE] <- "Reptile animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "snake" == TRUE] <- "Reptile animals and products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Pantherophis guttatus" == TRUE] <- "Reptile animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "green tree python" == TRUE] <- "Reptile animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "green tree python" == TRUE] <- "Reptile animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "python" == TRUE] <- "Reptile animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Python" == TRUE] <- "Reptile animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "reptilia" == TRUE] <- "Reptile animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "chamelion" == TRUE] <- "Reptile animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Dragon" == TRUE] <- "Reptile animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "dragon" == TRUE] <- "Reptile animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "lizard" == TRUE] <- "Reptile animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "reptile" == TRUE] <- "Reptile animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Sistrurus miliarius" == TRUE] <- "Reptile animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Alligator" == TRUE] <- "Reptile animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "alligator" == TRUE] <- "Reptile animals and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Iguana iguana" == TRUE] <- "Reptile animals and related"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "air" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Air" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "tank" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Tank" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "pond" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Pond" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "river" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Soil" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "soil" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Floor" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "floor" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "rinse water" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "irrigation water" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "forest" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Unit" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "unit" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "aquarium" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Aquarium" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "reservoir" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "GROUND" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "room" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "lake" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Lake" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "water" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Water" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Wall" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "wall" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "vacuum" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Vacuum" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "stage" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Ocean" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "ocean" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "door" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "creek" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Creek" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "vent" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Vent" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "farm" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "forest" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "barn" == TRUE] <- "Environment"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Barn" == TRUE] <- "Environment"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "oregano" == TRUE] <- "Seasoning"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "basil" == TRUE] <- "Seasoning"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Oregano" == TRUE] <- "Seasoning"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Basil" == TRUE] <- "Seasoning"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "paprika" == TRUE] <- "Seasoning"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Paprika" == TRUE] <- "Seasoning"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "rosemary" == TRUE] <- "Seasoning"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Rosemary" == TRUE] <- "Seasoning"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "tri pepper mixture" == TRUE] <- "Seasoning"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "powder" == TRUE] <- "Seasoning"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "thyme" == TRUE] <- "Seasoning"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "cumin" == TRUE] <- "Seasoning"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "sage" == TRUE] <- "Seasoning"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Sage" == TRUE] <- "Seasoning"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Spice" == TRUE] <- "Seasoning"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "spice" == TRUE] <- "Seasoning"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "soy" == TRUE] <- "Soy and bean products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Soy" == TRUE] <- "Soy and bean products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "bean" == TRUE] <- "Soy and bean products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Bean" == TRUE] <- "Soy and bean products"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "lettuce" == TRUE] <- "Vegetables: leafy green"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "kale" == TRUE] <- "Vegetables: leafy green"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "spinach" == TRUE] <- "Vegetables: leafy green"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Spinach" == TRUE] <- "Vegetables: leafy green"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "leaf" == TRUE] <- "Vegetables: leafy green"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "lolla rosa" == TRUE] <- "Vegetables: leafy green"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "salad" == TRUE] <- "Vegetables: leafy green"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Salad" == TRUE] <- "Vegetables: leafy green"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "sprout" == TRUE] <- "Vegetables: leafy green"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Sprout" == TRUE] <- "Vegetables: leafy green"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "cabbage" == TRUE] <- "Vegetables: cruciferous"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Mushroom" == TRUE] <- "Vegetables: fungi"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "mushroom" == TRUE] <- "Vegetables: fungi"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "green pepper" == TRUE] <- "Vegetables: pepper"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "bell pepper" == TRUE] <- "Vegetables: pepper"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "red pepper" == TRUE] <- "Vegetables: pepper"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "chili pepper" == TRUE] <- "Vegetable: pepper"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "pepper" == TRUE] <- "Vegetables: pepper"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Pepper" == TRUE] <- "Vegetables: pepper"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "hot pepper" == TRUE] <- "Vegetables: pepper"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "cucumber" == TRUE] <- "Vegetables: marrow"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Cucumber" == TRUE] <- "Vegetables: marrow"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "asparagus" == TRUE] <- "Vegetables: edible plant stem"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "celery" == TRUE] <- "Vegetables: edible plant stem"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Celery" == TRUE] <- "Vegetables: edible plant stems"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "corn" == TRUE] <- "Vegetables: starchy"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Corn" == TRUE] <- "Vegetables: starchy"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "pea" == TRUE] <- "Vegetables: starchy"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Pea" == TRUE] <- "Vegetables: starchy"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "radish" == TRUE] <- "Vegetables: root"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Chard" == TRUE] <- "Vegetables: root"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "chard" == TRUE] <- "Vegetables: root"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "fresh parsley" == TRUE] <- "Vegetables: herbs"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "cilantro" == TRUE] <- "Vegetables: herbs"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "coriander" == TRUE] <- "Vegetables: herbs"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Coriander" == TRUE] <- "Vegetables: herbs"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "veget" == TRUE] <- "Vegetables and their products"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "onion" == TRUE] <- "Vegetables: allium"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "onion" == TRUE] <- "Vegetables: allium"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "ginger" == TRUE] <- "Vegetables: allium "
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Ginger" == TRUE] <- "Vegetables: allium"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Lemon" == TRUE] <- "Fruits and their products: citrus"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "lemon" == TRUE] <- "Fruits and their products: citrus"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "lime" == TRUE] <- "Fruits and their products: citrus"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "papaya" == TRUE] <- "Fruits: tropical and exotic "
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "mango" == TRUE] <- "Fruits: tropical and exotic"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Mango" == TRUE] <- "Fruits: tropical and exotic"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "cantaloupe" == TRUE] <- "Fruits: melons"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Cantaloupe" == TRUE] <- "Fruits: melons"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "avocado" == TRUE] <- "Fruits: tomatoes and avocados"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Avocado" == TRUE] <- "Fruits: tomatoes and avocados"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "tomato" == TRUE] <- "Fruits: tomatoes and avocados"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Tomato" == TRUE] <- "Fruits: tomatoes and avocados"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "blueberry" == TRUE] <- "Fruits: berries"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Blueberry" == TRUE] <- "Fruits: berries"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "grape" == TRUE] <- "Fruits: berries"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "fruit" == TRUE] <- "Fruits and their products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Fruit" == TRUE] <- "Fruits and their products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "apple" == TRUE] <- "Fruits: apples and pears"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Apple" == TRUE] <- "Fruits: apples and pears"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "wheat" == TRUE] <- "Grain and their products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "rice" == TRUE] <- "Grain and their products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Rice" == TRUE] <- "Grain and their products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "oat" == TRUE] <- "Grain and their products"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "almond" == TRUE] <- "Nut seeds and their products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "nut" == TRUE] <- "Nut seeds and their products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Nut" == TRUE] <- "Nut seeds and their products"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "seed" == TRUE] <- "Nut seeds and their products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Seed" == TRUE] <- "Nut seeds and their products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "pecan" == TRUE] <- "Nut seeds and their products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Pecan" == TRUE] <- "Nut seeds and their products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Pistachio" == TRUE] <- "Nut seeds and their products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "pistachio" == TRUE] <- "Nut seeds and their products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "cashew" == TRUE] <- "Nut seeds and their products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Cashew" == TRUE] <- "Nut seeds and their products"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Sponge" == TRUE] <- "Algae and their products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "sponge" == TRUE] <- "Algae and their products"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "chocolate" == TRUE] <- "Candy, chocolate, honey and its products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "sugar" == TRUE] <- "Candy, chocolate, honey and its products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "candy" == TRUE] <- "Candy, chocolate, honey and its products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "honey" == TRUE] <- "Candy, chocolate, honey and its products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Chocolate" == TRUE] <- "Candy, chocolate, honey and its products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Sugar" == TRUE] <- "Candy, chocolate, honey and its products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Candy" == TRUE] <- "Candy, chocolate, honey and its products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "MARSHMALLOW" == TRUE] <- "Candy, chocolate, honey and its products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Marshmallow" == TRUE] <- "Candy, chocolate, honey and its products"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "marshmallow" == TRUE] <- "Candy, chocolate, honey and its products"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "juice" == TRUE] <- "Drinks and frozen beverages"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Juice" == TRUE] <- "Drinks and frozen beverages"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "tea" == TRUE] <-"Drinks and frozen beverages"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Tea" == TRUE] <-"Drinks and frozen beverages"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "pesto" == TRUE] <-"Processed food and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "ready" == TRUE] <-"Processed food and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "raw food" == TRUE] <-"Processed food and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "mayo" == TRUE] <-"Processed food and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Mayo" == TRUE] <-"Processed food and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "biscuit" == TRUE] <-"Processed food and related"

sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "burger" == TRUE] <-"Processed food and related"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "natural and artificial vanilla" == TRUE] <-"Processed food and related"


sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "other" == TRUE] <- "Other: unspecified source"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "urine culturer" == TRUE] <- "Other: unspecified source"
sal_usa$Isolation.source.category[sal_usa$Isolation.source.category%like% "Referred Culture" == TRUE] <- "Other: unspecified source"

length(unique(sal_usa$Isolation.source.category)) #1044
unique(sal_usa$Isolation.source.category)

write.csv(sal_usa,"source_salmonella_US.csv", row.names = FALSE)


