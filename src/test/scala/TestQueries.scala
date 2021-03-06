import cynthia.lang._

object TestQueries {
  val queries =
    List(
      // Query 1
      SetRes(
        Union(
          New("Listing", Seq()),
          New("Listing", Seq())
        )
      ),
      // Query 2
      AggrRes(
        Seq(
          FieldDecl(Count(None), "count", IntF)
        ),
        Union(
          New("Listing", Seq()),
          Union(
            New("Listing", Seq()),
            New("Listing", Seq())
          )
        )
      ),
      // Query3
      SetRes(
        Apply(
          Filter(
            Not(
              Eq("Listing.foo", Constant("bar", Quoted))
            )
          ),
          New("Listing", Seq())
        )
      ),
      // Query 4
      SetRes(
        Apply(
          Sort(
            Seq(("Listing.yearly_rent", Asc))
          ),
          Union(
            New("Listing", Seq()),
            New("Listing", Seq())
          )
        )
      ),
      // Query 5
      SetRes(
        Apply(
          Sort(
            Seq(
              ("Listing.foo", Desc),
              ("Listing.sale_price", Asc)
            )
          ),
          New("Listing", Seq())
        )
      ),
      // Query 6
      AggrRes(
        Seq(
          FieldDecl(Count(None), "count", IntF)
        ),
        Union(
          New("Listing", Seq()),
          Union(
            New("Listing", Seq()),
            New("Listing", Seq())
          )
        )
      ),
      // Query 7
      SetRes(
        Apply(
          Filter(
            And(
              Not(
                Eq("Listing.foo", Constant("bar", Quoted))
              ),
              Gte("Listing.sale_price", Constant("100", UnQuoted))
            )
          ),
          New("Listing", Seq())
        )
      ),
      // Query 8
      AggrRes(
        Seq(
          FieldDecl(Count(None), "count", IntF)
        ),
        New("Listing", Seq())
      ),
      // Query 9
      AggrRes(
        Seq(
          FieldDecl(Sum(F("Listing.sale_price")), "sum_sale", DoubleF),
          FieldDecl(Max(F("Listing.sale_price")), "max_sale", DoubleF),
          FieldDecl(Min(F("Listing.sale_price")), "min_sale", DoubleF),
          FieldDecl(Avg(F("Listing.sale_price")), "avg_sale", DoubleF)
        ),
        New("Listing", Seq())
      ),
      // Query 10
      AggrRes(
        Seq(
          FieldDecl(
            Add(
              Mul(
                Sum(F("Listing.sale_price")),
                Avg(F("Listing.yearly_rent"))
              ),
              Div(
                Max(F("Listing.sale_price")),
                Min(F("Listing.sale_price"))
              )
            ),
            "complex_add",
            DoubleF
          ),
          FieldDecl(Sum(F("Listing.yearly_rent")), "yearly", DoubleF),
          FieldDecl(
            Sub(
              Min(F("Listing.yearly_rent")),
              Add(
                Avg(F("Listing.sale_price")),
                Max(F("Listing.sale_price"))
              )
            ),
            "complex_sub",
            DoubleF
          )
        ),
        New("Listing", Seq())
      ),
      // Query 11
      AggrRes(
        Seq(
          FieldDecl(Max(F("Listing.foo")), "max", StringF),
          FieldDecl(Min(F("Listing.foo")), "min", StringF)
        ),
        New("Listing", Seq())
      ),
      // Query 12
      AggrRes(
        Seq(
          FieldDecl(
            Sum(
              Sub(
                F("Listing.yearly_rent"),
                F("Listing.sale_price")
              )
            ),
            "sum",
            DoubleF
          )
        ),
        New("Listing", Seq())
      ),
      // Query 13
      SetRes(
        Apply(
          Filter(
            Gte("custom", Constant("50", UnQuoted))
          ),
          New(
            "Listing",
            Seq(
              FieldDecl(
                Add(
                  F("Listing.yearly_rent"),
                  F("Listing.sale_price")
                ),
                "custom",
                DoubleF
              )
            )
          )
        )
      ),
      // Query 14
      SetRes(
        Apply(
          Filter(
            And(
              Eq("custom", Constant("20", UnQuoted)),
              Eq("text", Constant("foobar", Quoted))
            )
          ),
          New(
            "Listing",
            Seq(
              FieldDecl(
                Add(
                  Constant("5", UnQuoted),
                  Constant("15", UnQuoted)
                ),
                "custom",
                IntF
              ),
              FieldDecl(
                Constant("foobar", Quoted),
                "text",
                StringF
              )
            )
          )
        )
      ),
      // Query 15
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(F("Listing.sale_price"), "sales", DoubleF),
            FieldDecl(Sum(F("Listing.yearly_rent")), "sum", DoubleF)
          )
        )
      ),
      // Query 16
      SetRes(
        Apply(
          Filter(Gte("sales", Constant("1", UnQuoted))),
          New(
            "Listing",
            Seq(
              FieldDecl(
                Mul(
                  Constant("10", UnQuoted),
                  Div(
                    Constant("5", UnQuoted),
                    F("Listing.sale_price")
                  )
                ),
                "sales",
                DoubleF
              ),
              FieldDecl(Sum(F("Listing.yearly_rent")), "sum", DoubleF)
            )
          )
        )
      ),
      // Query 17
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(F("Listing.sale_price"), "sales", DoubleF),
            FieldDecl(
              Avg(
                Mul(
                  F("Listing.sale_price"),
                  F("Listing.sale_price")
                )
              ),
              "squared",
              DoubleF
            )
          )
        )
      ),
      // Query 18
      SetRes(
        Apply(
          Filter(Gte("max", Add(Constant("10", UnQuoted), F("sales")))),
          New(
            "Listing",
            Seq(
              FieldDecl(F("Listing.sale_price"), "sales", DoubleF),
              FieldDecl(
                Max(
                  Add(
                    F("Listing.yearly_rent"),
                    Constant("10", UnQuoted)
                  )
                ),
                "max",
                DoubleF
              )
            )
          )
        )
      ),
      // Query 19
      SetRes(
        Apply(
          Filter(
            And(
              Eq("fooF", Constant("baz", Quoted)),
              Gte("max", Add(Constant("10", UnQuoted), F("sales")))
            )
          ),
          New(
            "Listing",
            Seq(
              FieldDecl(F("Listing.foo"), "fooF", StringF),
              FieldDecl(F("Listing.sale_price"), "sales", DoubleF),
              FieldDecl(
                Max(
                  Add(
                    F("Listing.yearly_rent"),
                    Constant("10", UnQuoted)
                  )
                ),
                "max",
                DoubleF
              )
            )
          )
        )
      ),
      // Query 20
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(F("Listing.sale_price"), "sales", DoubleF),
            FieldDecl(
              Mul(
                F("Listing.sale_price"),
                F("Listing.sale_price")
              ),
              "mul",
              DoubleF,
              hidden = true
            ),
            FieldDecl(
              Sub(
                Avg(F("mul")),
                Constant("10", UnQuoted)
              ),
              "squared",
              DoubleF
            )
          )
        )
      ),
      // Query 21
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(F("Listing.sale_price"), "sales", DoubleF),
            FieldDecl(
              Mul(
                F("Listing.sale_price"),
                F("Listing.sale_price")
              ),
              "mul",
              DoubleF,
              hidden = true
            ),
            FieldDecl(Avg(F("mul")), "squared", DoubleF)
          )
        )
      ),
      // Query 22
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(F("Listing.sale_price"), "sales", DoubleF),
            FieldDecl(F("Listing.foo"), "fooF", StringF),
            FieldDecl(
              Mul(
                F("Listing.sale_price"),
                F("Listing.sale_price")
              ),
              "mul",
              DoubleF,
              hidden = true
            ),
            FieldDecl(Avg(F("mul")), "squared", DoubleF)
          )
        )
      ),
      // Query 23
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(
              Mul(
                Avg(F("Listing.yearly_rent")),
                Avg(F("Listing.sale_price"))
              ),
              "mul",
              DoubleF
            )
          )
        )
      ),
      // Query 24
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(F("Listing.sale_price"), "V", DoubleF, false),
            FieldDecl(Mul(F("Listing.id"), F("V")), "jjG", DoubleF, false),
            FieldDecl(
              Mul(
                Div(F("Listing.id"), F("jjG")),
                Min(Sub(F("V"), Constant("7", UnQuoted)))
              ),
              "qmIqh",
              DoubleF,
              false
            )
          )
        )
      ),
      // Query 25
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Constant("JB", Quoted), "Jble", StringF, false),
            FieldDecl(Constant("1", UnQuoted), "cn", IntF, true),
            FieldDecl(
              Min(
                Add(Constant("qdbiycgpD", Quoted), Constant("3WnBi", Quoted))
              ),
              "FJOKGoi",
              DoubleF,
              false
            )
          )
        )
      ),
      // Query 26
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Constant("EcwE", Quoted), "wduesdvc", StringF, false),
            FieldDecl(Sub(F("wduesdvc"), F("wduesdvc")), "rrW", DoubleF, false),
            FieldDecl(
              Add(Constant("2", UnQuoted), F("Listing.sale_price")),
              "bCGVwr",
              DoubleF,
              false
            )
          )
        )
      ),
      // Query 27
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(
              Sub(F("Listing.sale_price"), F("Listing.sale_price")),
              "GTfzOMrj",
              DoubleF,
              true
            ),
            FieldDecl(
              Sub(F("Listing.sale_price"), F("GTfzOMrj")),
              "lqA",
              DoubleF,
              false
            ),
            FieldDecl(Min(F("Listing.sale_price")), "Rp", DoubleF, false),
            FieldDecl(Sum(F("GTfzOMrj")), "blny", DoubleF, false)
          )
        )
      ),
      // Query 28
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Constant("3", UnQuoted), "WXDdG", IntF, false),
            FieldDecl(
              Mul(
                Sum(Add(F("Listing.id"), Constant("2", UnQuoted))),
                F("Listing.id")
              ),
              "CBG",
              DoubleF,
              false
            )
          )
        )
      ),
      // Query 29
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(
              Add(
                Mul(
                  Constant("3", UnQuoted),
                  Sub(
                    Add(F("Listing.yearly_rent"), F("Listing.id")),
                    Sum(Constant("obAoS5v", Quoted))
                  )
                ),
                Constant("bfJWBP7p", Quoted)
              ),
              "QutFiZgOg",
              DoubleF,
              false
            ),
            FieldDecl(
              Avg(Sub(F("Listing.sale_price"), Constant("4", UnQuoted))),
              "SsJNcy",
              DoubleF,
              false
            ),
            FieldDecl(F("Listing.sale_price"), "wdVXax", DoubleF, false),
            FieldDecl(
              Sub(
                Add(F("Listing.id"), Min(F("Listing.sale_price"))),
                Div(F("SsJNcy"), Sub(F("SsJNcy"), Constant("HYQvPq", Quoted)))
              ),
              "wmWMTQf",
              DoubleF,
              false
            )
          )
        )
      ),
      // Query 30
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Constant("6quxR", Quoted), "y", StringF, false),
            FieldDecl(
              Sum(Div(F("y"), Constant("2", UnQuoted))),
              "WIhPq",
              DoubleF,
              false
            )
          )
        )
      ),
      // Query 31
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Max(F("Listing.sale_price")), "Bypp", DoubleF, true),
            FieldDecl(F("Bypp"), "XcRfBTT", DoubleF, false),
            FieldDecl(Constant("duySFSo3w", Quoted), "PsJ", StringF, true)
          )
        )
      ),
      // Query 32
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(
              Add(
                Sub(
                  Add(
                    F("Listing.id"),
                    Min(Mul(F("Listing.sale_price"), F("Listing.sale_price")))
                  ),
                  Constant("7", UnQuoted)
                ),
                Add(
                  F("Listing.sale_price"),
                  Div(
                    Add(
                      Add(
                        Min(Constant("e5LIn", Quoted)),
                        F("Listing.sale_price")
                      ),
                      Count(Some(F("Listing.id")))
                    ),
                    F("Listing.sale_price")
                  )
                )
              ),
              "heTkuJqO",
              DoubleF,
              true
            ),
            FieldDecl(F("heTkuJqO"), "BIKnH", DoubleF, false)
          )
        )
      ),
      // Query 33
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Avg(F("Listing.id")), "Szgw", DoubleF, false),
            FieldDecl(Constant("VWSKU7", Quoted), "K", StringF, true),
            FieldDecl(F("Listing.sale_price"), "ee", DoubleF, false)
          )
        )
      ),
      // Query 34
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(
              Sub(Min(F("Listing.sale_price")), F("Listing.sale_price")),
              "ppVMYfS",
              DoubleF,
              true
            )
          )
        )
      ),
      // Query 35
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Count(Some(F("Listing.id"))), "QpUmZQeX", IntF, false),
            FieldDecl(
              Mul(Constant("p4d", Quoted), Constant("qYCGT", Quoted)),
              "dvdddN",
              DoubleF,
              true
            )
          )
        )
      ),
      // Query 36
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(Count(Some(F("Listing.id"))), "jJqOpWLil", IntF, true),
            FieldDecl(F("Listing.sale_price"), "ij", DoubleF, true),
            FieldDecl(F("jJqOpWLil"), "FPCwjJlb", IntF, false)
          )
        )
      ),
      // Query 37
      SetRes(
        New(
          "Listing",
          Seq(
            FieldDecl(
              Div(Min(Constant("0", UnQuoted)), F("Listing.sale_price")),
              "yVmbvGHWe",
              DoubleF,
              true
            ),
            FieldDecl(Add(F("yVmbvGHWe"), F("yVmbvGHWe")), "Te", DoubleF, false)
          )
        )
      ),
      // Query 38
      SetRes(
        New(
          "Listing",
          List(
            FieldDecl(Constant("6", UnQuoted), "ORsHBgQ", IntF, false),
            FieldDecl(
              Div(Sum(Constant("pD1B", Quoted)), F("Listing.sale_price")),
              "othaPT",
              DoubleF,
              false
            ),
            FieldDecl(F("othaPT"), "gOtzP", DoubleF, false),
            FieldDecl(Avg(Constant("AOjkd", Quoted)), "S", DoubleF, true),
            FieldDecl(Constant("4", UnQuoted), "x", IntF, true),
            FieldDecl(F("gOtzP"), "G", DoubleF, true)
          )
        )
      ),
      // Query 39
      SetRes(
        New(
          "Listing",
          List(
            FieldDecl(
              Avg(Constant("1", UnQuoted)),
              "irOHtSumD",
              DoubleF,
              false
            ),
            FieldDecl(F("irOHtSumD"), "jiAtXcec", DoubleF, false)
          )
        )
      ),
      // Query 40
      SetRes(
        New(
          "Listing",
          List(
            FieldDecl(Sum(F("Listing.yearly_rent")), "eqQy", DoubleF, false),
            FieldDecl(F("eqQy"), "rQe", DoubleF, true),
            FieldDecl(Count(Some(F("Listing.id"))), "dvcVCurv", IntF, true),
            FieldDecl(F("eqQy"), "I", DoubleF, false),
            FieldDecl(F("Listing.sale_price"), "TPH", DoubleF, true),
            FieldDecl(Constant("5", UnQuoted), "bqZCpnWO", IntF, true)
          )
        )
      ),
      // Query 41
      SetRes(
        New(
          "Listing",
          List(
            FieldDecl(
              Sub(F("Listing.sale_price"), F("Listing.yearly_rent")),
              "EoDFfvD",
              DoubleF,
              true
            ),
            FieldDecl(Min(Constant("8", Quoted)), "aJZXPcub", StringF, false),
            FieldDecl(
              Div(F("Listing.sale_price"), F("EoDFfvD")),
              "KWpgDGBLp",
              DoubleF,
              false
            )
          )
        )
      ),
      // Query 42
      SetRes(
        New(
          "Listing",
          List(
            FieldDecl(
              Div(
                Sub(
                  Constant("M0rf", Quoted),
                  Mul(F("Listing.sale_price"), Max(F("Listing.yearly_rent")))
                ),
                F("Listing.sale_price")
              ),
              "nbos",
              DoubleF,
              true
            ),
            FieldDecl(
              Mul(
                Div(
                  Count(Some(F("Listing.id"))),
                  Div(F("nbos"), Constant("7G", Quoted))
                ),
                Add(F("nbos"), Constant("ilkQN", Quoted))
              ),
              "lxg",
              DoubleF,
              false
            ),
            FieldDecl(Constant("L", Quoted), "xoOkrOnc", StringF, false),
            FieldDecl(
              Div(Constant("5", UnQuoted), F("Listing.id")),
              "vHtTZ",
              DoubleF,
              true
            ),
            FieldDecl(F("xoOkrOnc"), "UBXB", StringF, false),
            FieldDecl(Constant("NZ2", Quoted), "SNzkSd", StringF, false),
            FieldDecl(Avg(F("Listing.yearly_rent")), "MdHFe", DoubleF, false)
          )
        )
      ),
      // Query 43
      SetRes(
        Apply(
          Sort(List(("cCqQb", Asc))),
          New(
            "Listing",
            List(
              FieldDecl(
                Div(F("Listing.yearly_rent"), F("Listing.sale_price")),
                "WrTj",
                DoubleF,
                false
              ),
              FieldDecl(Constant("5", UnQuoted), "MPi", IntF, true),
              FieldDecl(
                Count(Some(Constant("wAi8pi4f", Quoted))),
                "jcVbPimsG",
                IntF,
                false
              ),
              FieldDecl(F("WrTj"), "hj", DoubleF, true),
              FieldDecl(Constant("0", UnQuoted), "cCqQb", IntF, false)
            )
          )
        )
      ),
      AggrRes(
        List(FieldDecl(Count(Some(F("EfLr"))), "KiqJoUU", IntF, false)),
        New(
          "Listing",
          List(
            FieldDecl(
              Sub(
                Add(
                  F("Listing.id"),
                  Div(
                    Add(
                      Div(
                        Constant("9", UnQuoted),
                        Constant("n3zKcdj2", Quoted)
                      ),
                      Add(
                        Div(
                          Div(Constant("8", UnQuoted), Constant("1", UnQuoted)),
                          F("Listing.id")
                        ),
                        F("Listing.yearly_rent")
                      )
                    ),
                    Constant("px5me", Quoted)
                  )
                ),
                Sub(Constant("y1e8rj", Quoted), Constant("f1cay92kV", Quoted))
              ),
              "EfLr",
              DoubleF,
              false
            )
          )
        )
      ),
      AggrRes(
        List(FieldDecl(Count(Some(F("R"))), "R", IntF, false)),
        New(
          "Listing",
          List(
            FieldDecl(F("Listing.sale_price"), "R", DoubleF, false),
            FieldDecl(
              Div(F("Listing.sale_price"), F("R")),
              "vlrkx",
              DoubleF,
              false
            ),
            FieldDecl(F("vlrkx"), "mUoSMAxdl", DoubleF, false),
            FieldDecl(
              Div(
                Mul(Constant("8", UnQuoted), F("Listing.id")),
                Constant("PHoJX5b", Quoted)
              ),
              "uBnSWipsi",
              DoubleF,
              false
            ),
            FieldDecl(
              Div(
                Constant("9", UnQuoted),
                Add(Constant("", Quoted), Constant("1", UnQuoted))
              ),
              "OAYwxOOET",
              DoubleF,
              false
            )
          )
        )
      ),
      SetRes(New("Book", Seq())),
      SetRes(
        Apply(
          Sort(Seq(("Review.rating", Desc))),
          New("Review", Seq())
        )
      ),
      SetRes(
        Apply(
          Filter(Eq("Review.book.title", Constant("Random book", Quoted))),
          New("Review", Seq())
        )
      ),
      SetRes(
        Apply(
          Filter(Eq("Review.book.author.surname", Constant("Coecker", Quoted))),
          New("Review", Seq())
        )
      ),
      SetRes(
        Apply(
          Sort(Seq(("Review.book.title", Desc))),
          New("Review", Seq())
        )
      ),
      SetRes(
        Apply(
          Sort(Seq(("Review.book.title", Desc), ("Review.reviewer_name", Asc))),
          New("Review", Seq())
        )
      ),
      SetRes(
        Apply(
          Sort(Seq(("Review.book.title", Desc), ("Review.reviewer_name", Asc))),
          Apply(
            Filter(
              And(
                Gte("Review.rating", Constant("2", UnQuoted)),
                Lte("Review.rating", Constant("4", UnQuoted))
              )
            ),
            New("Review", Seq())
          )
        )
      ),
      FirstRes(
        Apply(
          Sort(Seq(("Review.book.title", Desc), ("Review.reviewer_name", Asc))),
          Apply(
            Filter(
              And(
                Gte("Review.rating", Constant("2", UnQuoted)),
                Contains("Review.book.author.surname", "o")
              )
            ),
            New("Review", Seq())
          )
        )
      ),
      SubsetRes(
        1,
        Some(3),
        Apply(
          Sort(Seq(("Review.book.title", Desc), ("Review.reviewer_name", Asc))),
          Apply(
            Filter(
              Gte("Review.rating", Constant("2", UnQuoted))
            ),
            New("Review", Seq())
          )
        )
      ),
      SetRes(
        Apply(
          Filter(
            And(
              Gte("Review.rating", Constant("2", UnQuoted)),
              Lte("Review.book.author.first_name", Constant("Z", Quoted))
            )
          ),
          New(
            "Review",
            Seq(
              FieldDecl(
                Mul(
                  F("Review.rating"),
                  Constant("-1", UnQuoted)
                ),
                "mul",
                DoubleF
              ),
              FieldDecl(F("Review.book.author.first_name"), "name", StringF)
            )
          )
        )
      ),
      SetRes(
        New(
          "Review",
          List(
            FieldDecl(Constant("3", UnQuoted), "dqBZvjQX", IntF, true),
            FieldDecl(
              F("Review.book.author.first_name"),
              "TbPEVGKp",
              StringF,
              false
            )
          )
        )
      ),
      SetRes(
        New(
          "Review",
          List(
            FieldDecl(Constant("3", UnQuoted), "dqBZvjQX", IntF, true),
            FieldDecl(
              F("Review.book.author.first_name"),
              "TbPEVGKp",
              StringF,
              false
            )
          )
        )
      ),
      SetRes(
        Apply(
          Sort(
            List(
              ("Review.content", Asc),
              ("Review.book.id", Asc),
              ("Review.id", Asc),
              ("Review.reviewer_name", Asc)
            )
          ),
          New(
            "Review",
            List(
              FieldDecl(
                Sub(
                  Constant("8", UnQuoted),
                  Sum(Add(Constant("t7nVx", Quoted), F("Review.content")))
                ),
                "DniIHgWk",
                DoubleF,
                false
              )
            )
          )
        )
      ),
      SetRes(
        New(
          "Author",
          List(
            FieldDecl(
              Add(
                Mul(
                  Sub(
                    Add(Min(Constant("T26", Quoted)), F("Author.first_name")),
                    Constant("5", UnQuoted)
                  ),
                  Constant("8", UnQuoted)
                ),
                Add(F("Author.first_name"), Avg(Constant("4", UnQuoted)))
              ),
              "YUA",
              DoubleF,
              false
            ),
            FieldDecl(
              Avg(Div(Constant("2BRyR3", Quoted), Constant("3", UnQuoted))),
              "oD",
              DoubleF,
              false
            ),
            FieldDecl(Constant("7", UnQuoted), "C", IntF, true)
          )
        )
      ),
      SetRes(
        Apply(
          Sort(
            List(
              ("Review.id", Asc),
              ("V", Desc),
              ("Review.reviewer_name", Desc),
              ("Review.content", Desc),
              ("Review.book.id", Desc)
            )
          ),
          New(
            "Review",
            List(FieldDecl(Constant("0", UnQuoted), "V", IntF, false))
          )
        )
      ),
      SetRes(
        Apply(
          Sort(
            List(("Book.author.id", Asc), ("Book.isbn", Asc), ("Book.id", Desc))
          ),
          New(
            "Book",
            List(
              FieldDecl(F("Book.title"), "e", StringF, true),
              FieldDecl(Constant("", Quoted), "YejRlb", StringF, true),
              FieldDecl(
                Sub(F("YejRlb"), F("Book.isbn")),
                "aqVzP",
                DoubleF,
                false
              ),
              FieldDecl(Constant("0", UnQuoted), "PYBKyS", IntF, true)
            )
          )
        )
      ),
      SetRes(
        Apply(
          Sort(
            List(("Book.isbn", Desc), ("Izi", Desc), ("Book.author.id", Asc))
          ),
          New(
            "Book",
            List(
              FieldDecl(
                Sub(F("Book.author.first_name"), F("Book.title")),
                "RvimPPZOm",
                DoubleF,
                false
              ),
              FieldDecl(Max(Constant("Xl950", Quoted)), "Izi", StringF, false)
            )
          )
        )
      ),
      SetRes(
        Apply(
          Sort(
            List(
              ("Book.isbn", Desc),
              ("Book.title", Desc),
              ("ODQw", Desc),
              ("Book.id", Desc)
            )
          ),
          New(
            "Book",
            List(
              FieldDecl(Constant("0", UnQuoted), "gLMHoT", IntF, true),
              FieldDecl(
                Add(Sum(F("Book.title")), Max(F("Book.isbn"))),
                "caxccB",
                DoubleF,
                false
              ),
              FieldDecl(Constant("8", UnQuoted), "VUV", IntF, false),
              FieldDecl(
                Sum(Div(F("gLMHoT"), Constant("VY", Quoted))),
                "ODQw",
                DoubleF,
                false
              )
            )
          )
        )
      ),
      AggrRes(
        List(FieldDecl(Count(Some(F("Review.content"))), "g", IntF, false)),
        New(
          "Review",
          List(
            FieldDecl(Constant("SBkK", Quoted), "vICp", StringF, false),
            FieldDecl(
              Add(F("Review.content"), Div(F("vICp"), F("vICp"))),
              "OIeH",
              DoubleF,
              false
            ),
            FieldDecl(F("vICp"), "sEwe", StringF, false),
            FieldDecl(
              Mul(F("Review.id"), F("Review.id")),
              "KB",
              DoubleF,
              false
            ),
            FieldDecl(Constant("4", UnQuoted), "JFSL", IntF, false)
          )
        )
      ),
      SetRes(
        New(
          "Review",
          List(
            FieldDecl(
              Sum(Div(F("Review.rating"), F("Review.book.title"))),
              "TkhJ",
              DoubleF,
              false
            ),
            FieldDecl(Sub(F("Review.rating"), F("TkhJ")), "II", DoubleF, true)
          )
        )
      ),
      SetRes(
        Apply(
          Sort(List(("TbPEVGKp", Desc))),
          Apply(
            Distinct(Some("TbPEVGKp")),
            New(
              "Review",
              List(
                FieldDecl(Constant("3", UnQuoted), "dqBZvjQX", IntF, true),
                FieldDecl(
                  F("Review.book.author.first_name"),
                  "TbPEVGKp",
                  StringF,
                  false
                )
              )
            )
          )
        )
      ),
      SetRes(
        Apply(
          Distinct(None),
          New(
            "Review",
            List(
              FieldDecl(Constant("3", UnQuoted), "dqBZvjQX", IntF, true),
              FieldDecl(
                F("Review.book.author.first_name"),
                "TbPEVGKp",
                StringF,
                false
              )
            )
          )
        )
      ),
      SetRes(
        Apply(
          Sort(List(("Review.book.author.first_name", Desc))),
          Apply(
            Distinct(Some("Review.book.author.first_name")),
            New(
              "Review",
              List(
                FieldDecl(Constant("3", UnQuoted), "dqBZvjQX", IntF, true),
                FieldDecl(
                  F("Review.book.author.first_name"),
                  "TbPEVGKp",
                  StringF,
                  false
                )
              )
            )
          )
        )
      )
    )
}
