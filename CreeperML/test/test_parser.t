  $ ./test_parser.exe <<- EOF
  > let fac n =
  > let rec helper n acc =
  > if n <= 1 then 
  > acc
  > else
  > helper (n - 1) (n * acc)
  > in
  > helper n 1
  > EOF
  [{ value =
     { rec_f = NoRec;
       l_v =
       { value = (LvValue "fac"); pos = {start_p: [: 1 0 4]; end_p: [: 1 0 7]}
         };
       args =
       [{ value = (LvValue "n"); pos = {start_p: [: 1 0 8]; end_p: [: 1 0 9]} }
         ];
       body =
       { value =
         { lets = [];
           expr =
           { value =
             EFun {
               lvalue =
               { value = (LvValue "n");
                 pos = {start_p: [: 1 0 8]; end_p: [: 1 0 9]} };
               body =
               { value =
                 { lets =
                   [{ value =
                      { rec_f = Rec;
                        l_v =
                        { value = (LvValue "helper");
                          pos = {start_p: [: 1 0 20]; end_p: [: 1 0 26]} };
                        args =
                        [{ value = (LvValue "n");
                           pos = {start_p: [: 1 0 27]; end_p: [: 1 0 28]} };
                          { value = (LvValue "acc");
                            pos = {start_p: [: 1 0 29]; end_p: [: 1 0 32]} }
                          ];
                        body =
                        { value =
                          { lets = [];
                            expr =
                            { value =
                              EFun {
                                lvalue =
                                { value = (LvValue "n");
                                  pos =
                                  {start_p: [: 1 0 27]; end_p: [: 1 0 28]} };
                                body =
                                { value =
                                  { lets = [];
                                    expr =
                                    { value =
                                      EFun {
                                        lvalue =
                                        { value = (LvValue "acc");
                                          pos =
                                          {start_p: [: 1 0 29]; end_p: [: 1 0 32]}
                                          };
                                        body =
                                        { value =
                                          { lets = [];
                                            expr =
                                            { value =
                                              EIfElse {
                                                cond =
                                                { value =
                                                  (EApply (
                                                     { value =
                                                       (EApply (
                                                          { value =
                                                            (EValue "<=");
                                                            pos =
                                                            {start_p: [: 1 0 40]; end_p: [: 1 0 42]}
                                                            },
                                                          { value =
                                                            (EValue "n");
                                                            pos =
                                                            {start_p: [: 1 0 38]; end_p: [: 1 0 39]}
                                                            }
                                                          ));
                                                       pos =
                                                       {start_p: [: 1 0 38]; end_p: [: 1 0 44]}
                                                       },
                                                     { value =
                                                       (ELiteral
                                                          { value = (LInt 1);
                                                            pos =
                                                            {start_p: [: 1 0 43]; end_p: [: 1 0 44]}
                                                            });
                                                       pos =
                                                       {start_p: [: 1 0 43]; end_p: [: 1 0 44]}
                                                       }
                                                     ));
                                                  pos =
                                                  {start_p: [: 1 0 38]; end_p: [: 1 0 44]}
                                                  };
                                                t_body =
                                                { value = (EValue "acc");
                                                  pos =
                                                  {start_p: [: 1 0 51]; end_p: [: 1 0 54]}
                                                  };
                                                f_body =
                                                { value =
                                                  (EApply (
                                                     { value =
                                                       (EApply (
                                                          { value =
                                                            (EValue "helper");
                                                            pos =
                                                            {start_p: [: 1 0 60]; end_p: [: 1 0 66]}
                                                            },
                                                          { value =
                                                            (EApply (
                                                               { value =
                                                                 (EApply (
                                                                    { value =
                                                                      (
                                                                      EValue
                                                                      "-");
                                                                      pos =
                                                                      {start_p: [: 1 0 70]; end_p: [: 1 0 71]}
                                                                      },
                                                                    { value =
                                                                      (
                                                                      EValue
                                                                      "n");
                                                                      pos =
                                                                      {start_p: [: 1 0 68]; end_p: [: 1 0 69]}
                                                                      }
                                                                    ));
                                                                 pos =
                                                                 {start_p: [: 1 0 68]; end_p: [: 1 0 73]}
                                                                 },
                                                               { value =
                                                                 (ELiteral
                                                                    { value =
                                                                      (
                                                                      LInt 1);
                                                                      pos =
                                                                      {start_p: [: 1 0 72]; end_p: [: 1 0 73]}
                                                                      });
                                                                 pos =
                                                                 {start_p: [: 1 0 72]; end_p: [: 1 0 73]}
                                                                 }
                                                               ));
                                                            pos =
                                                            {start_p: [: 1 0 68]; end_p: [: 1 0 73]}
                                                            }
                                                          ));
                                                       pos =
                                                       {start_p: [: 1 0 60]; end_p: [: 1 0 74]}
                                                       },
                                                     { value =
                                                       (EApply (
                                                          { value =
                                                            (EApply (
                                                               { value =
                                                                 (EValue "*");
                                                                 pos =
                                                                 {start_p: [: 1 0 78]; end_p: [: 1 0 79]}
                                                                 },
                                                               { value =
                                                                 (EValue "n");
                                                                 pos =
                                                                 {start_p: [: 1 0 76]; end_p: [: 1 0 77]}
                                                                 }
                                                               ));
                                                            pos =
                                                            {start_p: [: 1 0 76]; end_p: [: 1 0 83]}
                                                            },
                                                          { value =
                                                            (EValue "acc");
                                                            pos =
                                                            {start_p: [: 1 0 80]; end_p: [: 1 0 83]}
                                                            }
                                                          ));
                                                       pos =
                                                       {start_p: [: 1 0 76]; end_p: [: 1 0 83]}
                                                       }
                                                     ));
                                                  pos =
                                                  {start_p: [: 1 0 60]; end_p: [: 1 0 84]}
                                                  }};
                                              pos =
                                              {start_p: [: 1 0 35]; end_p: [: 1 0 84]}
                                              }
                                            };
                                          pos =
                                          {start_p: [: 1 0 34]; end_p: [: 1 0 84]}
                                          }};
                                      pos =
                                      {start_p: [: 1 0 12]; end_p: [: 1 0 84]}
                                      }
                                    };
                                  pos =
                                  {start_p: [: 1 0 12]; end_p: [: 1 0 84]} }};
                              pos = {start_p: [: 1 0 12]; end_p: [: 1 0 84]} }
                            };
                          pos = {start_p: [: 1 0 12]; end_p: [: 1 0 84]} }
                        };
                      pos = {start_p: [: 1 0 12]; end_p: [: 1 0 84]} }
                     ];
                   expr =
                   { value =
                     (EApply (
                        { value =
                          (EApply (
                             { value = (EValue "helper");
                               pos = {start_p: [: 1 0 88]; end_p: [: 1 0 94]} },
                             { value = (EValue "n");
                               pos = {start_p: [: 1 0 95]; end_p: [: 1 0 96]} }
                             ));
                          pos = {start_p: [: 1 0 88]; end_p: [: 1 0 96]} },
                        { value =
                          (ELiteral
                             { value = (LInt 1);
                               pos = {start_p: [: 1 0 97]; end_p: [: 1 0 98]} });
                          pos = {start_p: [: 1 0 97]; end_p: [: 1 0 98]} }
                        ));
                     pos = {start_p: [: 1 0 88]; end_p: [: 1 0 98]} }
                   };
                 pos = {start_p: [: 1 0 12]; end_p: [: 1 0 98]} }};
             pos = {start_p: [: 1 0 0]; end_p: [: 1 0 98]} }
           };
         pos = {start_p: [: 1 0 0]; end_p: [: 1 0 98]} }
       };
     pos = {start_p: [: 1 0 0]; end_p: [: 1 0 98]} }
    ]
