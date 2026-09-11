(ns culture.facts
  "Country-level regional-culture catalog for Uganda (UGA) -- national
  dishes, protected products, beverages, crafts, festivals and heritage
  sites, per ADR-2607171400 addendum 2 (cloud-itonami-municipality-
  culture-catalog Wave 1, in com-junkawasaki/root). Sibling namespace to
  `marketentry.facts` / `statute.facts` (ADR-2607141700); city-level
  counterparts live in the cloud-itonami-municipality-* repos.

  Catalog is keyed by UPPERCASE ISO3 (mirrors `statute.facts`); entries
  carry no :culture/municipality (that attribute is city-level only).

  Every entry cites a source URL that was actually fetched and read on
  :culture/retrieved-at -- never fabricated. Summaries state only what the
  cited source confirms. An item not in this table has NO spec-basis, full
  stop; extend `catalog`, do not invent an id/url.")

(def catalog
  "iso3 -> vector of culture entries."
  {"UGA"
   [{:culture/id "uga.dish.matoke"
     :culture/name "Matoke"
     :culture/country "UGA"
     :culture/kind :dish
     :culture/summary "Group of starchy triploid banana cultivars from the African Great Lakes region; the local name is synonymous with the word \"food\" in Uganda and is considered a national dish there, though it is also a staple crop in Kenya, Tanzania, Rwanda and Burundi."
     :culture/url "https://en.wikipedia.org/wiki/Matoke"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "uga.dish.rolex"
     :culture/name "Rolex"
     :culture/country "UGA"
     :culture/kind :dish
     :culture/summary "Popular Ugandan street food of a vegetable omelette wrapped in chapati, reflecting a fusion of South Asian culinary traditions introduced by Indian laborers during the colonial railway era with local ingredients."
     :culture/url "https://en.wikipedia.org/wiki/Rolex_(food)"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "uga.dish.luwombo"
     :culture/name "Luwombo"
     :culture/country "UGA"
     :culture/kind :dish
     :culture/summary "Traditional Ugandan stew or sauce prepared using smoked young banana leaves, originating in the Buganda Kingdom in 1887; originally reserved for royalty, now enjoyed across ethnic groups throughout Uganda."
     :culture/url "https://en.wikipedia.org/wiki/Luwombo"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "uga.beverage.waragi"
     :culture/name "Waragi"
     :culture/country "UGA"
     :culture/kind :beverage
     :culture/summary "Generic term in Uganda for domestic distilled beverages, particularly homemade gin, tracing to British soldiers' introduction of gin during the colonial era and later localized into the industrially-produced Uganda Waragi brand."
     :culture/url "https://en.wikipedia.org/wiki/Waragi"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "uga.craft.barkcloth"
     :culture/name "Barkcloth"
     :culture/country "UGA"
     :culture/kind :craft
     :culture/summary "Cloth manufactured in Buganda, Uganda for centuries from the bark of the ficus natalensis tree; Uganda's sole representative on the UNESCO Intangible Cultural Heritage Lists, proclaimed a Masterpiece of Oral and Intangible Heritage in 2005."
     :culture/url "https://en.wikipedia.org/wiki/Barkcloth"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "uga.festival.imbalu"
     :culture/name "Imbalu"
     :culture/country "UGA"
     :culture/kind :festival
     :culture/summary "Public circumcision ceremony practiced by the Bamasaba people of Uganda as a rite of passage for boys into manhood, held at the Mutoto cultural site near Mbale in even-numbered years and recognized by UNESCO as Intangible Cultural Heritage."
     :culture/url "https://en.wikipedia.org/wiki/Imbalu"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "uga.heritage.kasubi-tombs"
     :culture/name "Kasubi Tombs"
     :culture/country "UGA"
     :culture/kind :heritage
     :culture/summary "Burial grounds for four Buganda kings and royal family members in Kampala, Uganda; designated a UNESCO World Heritage Site in 2001, described as one of the most remarkable buildings using purely vegetal materials in sub-Saharan Africa."
     :culture/url "https://en.wikipedia.org/wiki/Kasubi_Tombs"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}
    {:culture/id "uga.heritage.bwindi-impenetrable-national-park"
     :culture/name "Bwindi Impenetrable National Park"
     :culture/country "UGA"
     :culture/kind :heritage
     :culture/summary "Protected area in southwestern Uganda and a UNESCO World Heritage Site, notable for protecting nearly half of the world's endangered mountain gorilla population along with over 1,000 plant species and 350 bird species."
     :culture/url "https://en.wikipedia.org/wiki/Bwindi_Impenetrable_National_Park"
     :culture/url-provenance :wikipedia-en
     :culture/retrieved-at "2026-07-17"}]})

(defn spec-basis [iso3] (get catalog iso3))

(defn coverage
  ([] (coverage (keys catalog)))
  ([iso3s]
   (let [have (filter catalog iso3s)
         missing (remove catalog iso3s)]
     {:requested (count iso3s)
      :covered (count have)
      :covered-jurisdictions (vec (sort have))
      :missing-jurisdictions (vec (sort missing))
      :note (str "cloud-itonami-iso3166-uga culture catalog "
                 "(ADR-2607171400 addendum 2, Wave 1): " (count (get catalog "UGA"))
                 " UGA entries, each with a fetched-and-read citation. "
                 "Extend `culture.facts/catalog`, never fabricate an id/url.")})))

(defn by-kind [iso3 kind]
  (filterv #(= (:culture/kind %) kind) (spec-basis iso3)))
