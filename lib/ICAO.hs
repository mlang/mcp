{-# LANGUAGE RecordWildCards #-}
module ICAO (ICAOCode(..), point, nearest) where

import Data.List (sortOn)
import Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Geo.Computations

point :: ICAOCode -> Point
point c = icaoPoint . fromJust $ Map.lookup c icaoData

nearest :: Point -> (ICAOCode, Vector)
nearest p = head . sortOn (fst . snd) $
            fmap vec <$> Map.assocs icaoData
 where vec ICAOData{..} = getVector p icaoPoint

data ICAOCode = AGGH
              | AGGL
              | AGGM
              | ANAU
              | AYMD
              | AYPY
              | AYWK
              | BGAM
              | BGAS
              | BGAT
              | BGBW
              | BGCO
              | BGDB
              | BGDH
              | BGEM
              | BGFH
              | BGGD
              | BGGH
              | BGHB
              | BGJH
              | BGJN
              | BGKK
              | BGKT
              | BGPC
              | BGSC
              | BGSF
              | BGTL
              | BIAR
              | BIEG
              | BIGR
              | BIHN
              | BIKF
              | BIRG
              | BIRK
              | BIST
              | BIVM
              | BIVO
              | CWAC
              | CWAF
              | CWAG
              | CWAH
              | CWAJ
              | CWAN
              | CWAQ
              | CWAR
              | CWAS
              | CWAV
              | CWAX
              | CWBA
              | CWBE
              | CWBG
              | CWBI
              | CWBJ
              | CWBK
              | CWBL
              | CWBM
              | CWBO
              | CWBR
              | CWBT
              | CWBV
              | CWBY
              | CWBZ
              | CWCA
              | CWCD
              | CWCF
              | CWCH
              | CWCI
              | CWCJ
              | CWCL
              | CWCM
              | CWCO
              | CWCU
              | CWCX
              | CWDA
              | CWDB
              | CWDC
              | CWDE
              | CWDF
              | CWDH
              | CWDI
              | CWDJ
              | CWDK
              | CWDL
              | CWDM
              | CWDO
              | CWDP
              | CWDQ
              | CWDS
              | CWDT
              | CWDU
              | CWDV
              | CWEA
              | CWEB
              | CWEC
              | CWEE
              | CWEF
              | CWEH
              | CWEI
              | CWEK
              | CWEL
              | CWEO
              | CWEP
              | CWEQ
              | CWER
              | CWES
              | CWEU
              | CWEV
              | CWEW
              | CWEZ
              | CWFD
              | CWFE
              | CWFF
              | CWFG
              | CWFJ
              | CWFN
              | CWFQ
              | CWFW
              | CWFX
              | CWFZ
              | CWGB
              | CWGD
              | CWGF
              | CWGH
              | CWGL
              | CWGM
              | CWGN
              | CWGP
              | CWGQ
              | CWGR
              | CWGT
              | CWGU
              | CWGW
              | CWGY
              | CWHA
              | CWHC
              | CWHE
              | CWHH
              | CWHI
              | CWHL
              | CWHM
              | CWHO
              | CWHP
              | CWHQ
              | CWHS
              | CWHT
              | CWHV
              | CWHW
              | CWHY
              | CWIC
              | CWID
              | CWIE
              | CWII
              | CWIJ
              | CWIK
              | CWIL
              | CWIO
              | CWIP
              | CWIQ
              | CWIS
              | CWIT
              | CWIW
              | CWIX
              | CWIZ
              | CWJA
              | CWJB
              | CWJC
              | CWJD
              | CWJH
              | CWJI
              | CWJM
              | CWJN
              | CWJO
              | CWJP
              | CWJR
              | CWJT
              | CWJW
              | CWJX
              | CWJY
              | CWJZ
              | CWKD
              | CWKE
              | CWKG
              | CWKH
              | CWKK
              | CWKM
              | CWKO
              | CWKW
              | CWKX
              | CWLA
              | CWLB
              | CWLE
              | CWLF
              | CWLG
              | CWLI
              | CWLM
              | CWLO
              | CWLP
              | CWLQ
              | CWLS
              | CWLT
              | CWLV
              | CWLX
              | CWLY
              | CWLZ
              | CWME
              | CWMH
              | CWMI
              | CWMJ
              | CWMM
              | CWMN
              | CWMP
              | CWMQ
              | CWMS
              | CWMT
              | CWMU
              | CWMV
              | CWMW
              | CWMX
              | CWMZ
              | CWNB
              | CWNC
              | CWND
              | CWNH
              | CWNK
              | CWNL
              | CWNM
              | CWNO
              | CWNP
              | CWNQ
              | CWNR
              | CWNT
              | CWNW
              | CWNX
              | CWNZ
              | CWOA
              | CWOB
              | CWOC
              | CWOD
              | CWOE
              | CWOH
              | CWOI
              | CWOK
              | CWOU
              | CWPA
              | CWPB
              | CWPC
              | CWPD
              | CWPF
              | CWPH
              | CWPI
              | CWPJ
              | CWPK
              | CWPL
              | CWPO
              | CWPQ
              | CWPR
              | CWPS
              | CWPU
              | CWQC
              | CWQE
              | CWQF
              | CWQG
              | CWQH
              | CWQK
              | CWQL
              | CWQM
              | CWQO
              | CWQP
              | CWQQ
              | CWQR
              | CWQS
              | CWQV
              | CWQW
              | CWQY
              | CWRA
              | CWRD
              | CWRF
              | CWRH
              | CWRJ
              | CWRN
              | CWRO
              | CWRP
              | CWRT
              | CWRU
              | CWRV
              | CWRW
              | CWRY
              | CWRZ
              | CWSA
              | CWSE
              | CWSF
              | CWSG
              | CWSH
              | CWSI
              | CWSK
              | CWSL
              | CWSP
              | CWSQ
              | CWSR
              | CWSS
              | CWST
              | CWSY
              | CWSZ
              | CWTA
              | CWTB
              | CWTC
              | CWTD
              | CWTE
              | CWTF
              | CWTG
              | CWTN
              | CWTO
              | CWTY
              | CWUM
              | CWUP
              | CWUR
              | CWUS
              | CWUT
              | CWUU
              | CWUW
              | CWUX
              | CWUY
              | CWVA
              | CWVC
              | CWVD
              | CWVH
              | CWVI
              | CWVK
              | CWVN
              | CWVO
              | CWVP
              | CWVQ
              | CWVT
              | CWVU
              | CWVY
              | CWWA
              | CWWB
              | CWWC
              | CWWE
              | CWWF
              | CWWK
              | CWWL
              | CWWN
              | CWWO
              | CWWP
              | CWWS
              | CWWV
              | CWWX
              | CWWZ
              | CWXA
              | CWXC
              | CWXH
              | CWXL
              | CWXM
              | CWXP
              | CWXV
              | CWXW
              | CWYE
              | CWYH
              | CWYJ
              | CWYK
              | CWYL
              | CWYM
              | CWYO
              | CWYY
              | CWZA
              | CWZB
              | CWZN
              | CWZQ
              | CWZS
              | CWZT
              | CWZV
              | CWZZ
              | CXBK
              | CXBO
              | CXCK
              | CXDE
              | CXDI
              | CXDW
              | CXEC
              | CXEG
              | CXGH
              | CXHF
              | CXKT
              | CXLC
              | CXLL
              | CXMD
              | CXMM
              | CXNM
              | CXOX
              | CXPV
              | CXQA
              | CXRH
              | CXSH
              | CXSR
              | CXSW
              | CXTN
              | CXTP
              | CXTV
              | CXWN
              | CXXX
              | CXZU
              | CYAH
              | CYAJ
              | CYAM
              | CYAW
              | CYAZ
              | CYBC
              | CYBD
              | CYBG
              | CYBK
              | CYBR
              | CYBU
              | CYBV
              | CYBX
              | CYCB
              | CYCD
              | CYCG
              | CYCH
              | CYCL
              | CYCO
              | CYCP
              | CYCT
              | CYCX
              | CYCY
              | CYDA
              | CYDC
              | CYDF
              | CYDN
              | CYDP
              | CYED
              | CYEG
              | CYEN
              | CYET
              | CYEV
              | CYFB
              | CYFC
              | CYFS
              | CYGK
              | CYGL
              | CYGM
              | CYGP
              | CYGQ
              | CYGR
              | CYGV
              | CYGW
              | CYGX
              | CYHA
              | CYHB
              | CYHE
              | CYHM
              | CYHU
              | CYHY
              | CYHZ
              | CYIO
              | CYIV
              | CYJT
              | CYKA
              | CYKF
              | CYKJ
              | CYKL
              | CYKY
              | CYLJ
              | CYLL
              | CYLW
              | CYMA
              | CYMD
              | CYMJ
              | CYMM
              | CYMO
              | CYMT
              | CYNA
              | CYNE
              | CYNM
              | CYOD
              | CYOJ
              | CYOW
              | CYOY
              | CYPA
              | CYPE
              | CYPG
              | CYPQ
              | CYPR
              | CYPW
              | CYPY
              | CYPZ
              | CYQA
              | CYQB
              | CYQD
              | CYQF
              | CYQG
              | CYQH
              | CYQI
              | CYQK
              | CYQL
              | CYQM
              | CYQQ
              | CYQR
              | CYQT
              | CYQU
              | CYQV
              | CYQW
              | CYQX
              | CYQY
              | CYQZ
              | CYRB
              | CYRJ
              | CYRL
              | CYRM
              | CYRT
              | CYRV
              | CYSB
              | CYSC
              | CYSD
              | CYSF
              | CYSJ
              | CYSL
              | CYSM
              | CYSU
              | CYTE
              | CYTH
              | CYTL
              | CYTR
              | CYTS
              | CYTZ
              | CYUA
              | CYUB
              | CYUI
              | CYUJ
              | CYUK
              | CYUL
              | CYUQ
              | CYUS
              | CYUX
              | CYVC
              | CYVO
              | CYVP
              | CYVQ
              | CYVR
              | CYVV
              | CYWA
              | CYWG
              | CYWK
              | CYWL
              | CYXC
              | CYXD
              | CYXE
              | CYXH
              | CYXJ
              | CYXL
              | CYXR
              | CYXS
              | CYXT
              | CYXU
              | CYXX
              | CYXY
              | CYXZ
              | CYYB
              | CYYC
              | CYYD
              | CYYE
              | CYYF
              | CYYG
              | CYYJ
              | CYYL
              | CYYN
              | CYYQ
              | CYYR
              | CYYT
              | CYYU
              | CYYW
              | CYYY
              | CYYZ
              | CYZE
              | CYZF
              | CYZH
              | CYZP
              | CYZR
              | CYZS
              | CYZT
              | CYZU
              | CYZV
              | CYZW
              | CYZX
              | CYZY
              | CZDI
              | CZOC
              | CZPK
              | CZST
              | CZUB
              | CZUE
              | CZUM
              | DAAD
              | DAAE
              | DAAG
              | DAAJ
              | DAAP
              | DAAS
              | DAAV
              | DAAY
              | DABB
              | DABC
              | DABP
              | DABS
              | DABT
              | DAFI
              | DAOB
              | DAOF
              | DAOI
              | DAON
              | DAOO
              | DAOR
              | DAOV
              | DAUA
              | DAUB
              | DAUE
              | DAUG
              | DAUH
              | DAUK
              | DAUL
              | DAUO
              | DAUT
              | DAUU
              | DAUZ
              | DBBB
              | DBBC
              | DBBK
              | DBBN
              | DBBP
              | DBBS
              | DFCC
              | DFCO
              | DFCP
              | DFEE
              | DFEF
              | DFFD
              | DFOD
              | DFOG
              | DFOO
              | DGAA
              | DGAD
              | DGAH
              | DGAK
              | DGAS
              | DGAT
              | DGKA
              | DGKK
              | DGLB
              | DGLE
              | DGLN
              | DGLW
              | DGLY
              | DGSB
              | DGSI
              | DGSN
              | DGSW
              | DGTK
              | DGTX
              | DIAD
              | DIAP
              | DIBK
              | DIBU
              | DIDK
              | DIDL
              | DIGA
              | DIKO
              | DIMN
              | DIOD
              | DISP
              | DISS
              | DITB
              | DIYO
              | DNBI
              | DNCA
              | DNEN
              | DNGU
              | DNIB
              | DNIL
              | DNJO
              | DNKA
              | DNKN
              | DNMA
              | DNMK
              | DNMM
              | DNOS
              | DNPO
              | DNSO
              | DNYO
              | DNZA
              | DRRB
              | DRRG
              | DRRI
              | DRRL
              | DRRM
              | DRRN
              | DRRT
              | DRZA
              | DRZF
              | DRZG
              | DRZM
              | DRZR
              | DTKA
              | DTTA
              | DTTB
              | DTTD
              | DTTF
              | DTTG
              | DTTJ
              | DTTK
              | DTTL
              | DTTM
              | DTTN
              | DTTR
              | DTTX
              | DTTZ
              | DXAK
              | DXMG
              | DXNG
              | DXSK
              | DXTA
              | DXXX
              | EBAW
              | EBBE
              | EBBL
              | EBBR
              | EBBT
              | EBBX
              | EBCI
              | EBCV
              | EBDT
              | EBFN
              | EBFS
              | EBGT
              | EBLB
              | EBLG
              | EBMB
              | EBMT
              | EBOS
              | EBSP
              | EBST
              | EBSU
              | EBTN
              | EBWE
              | EBZW
              | EDDB
              | EDDC
              | EDDE
              | EDDF
              | EDDG
              | EDDH
              | EDDI
              | EDDK
              | EDDL
              | EDDM
              | EDDN
              | EDDP
              | EDDR
              | EDDS
              | EDDT
              | EDDV
              | EDDW
              | EDFH
              | EDFM
              | EDHI
              | EDHL
              | EDLP
              | EDLW
              | EDMA
              | EDNY
              | EDOP
              | EDQD
              | EDQM
              | EDTD
              | EDTZ
              | EDVE
              | EDVK
              | EDXW
              | EDZE
              | EEKA
              | EFHA
              | EFHF
              | EFHK
              | EFHL
              | EFHT
              | EFIV
              | EFJO
              | EFJY
              | EFKA
              | EFKE
              | EFKI
              | EFKK
              | EFKS
              | EFKU
              | EFLP
              | EFMA
              | EFMI
              | EFOU
              | EFPE
              | EFPO
              | EFPU
              | EFRO
              | EFSA
              | EFSO
              | EFSU
              | EFTP
              | EFTU
              | EFUT
              | EFVA
              | EFVI
              | EGAA
              | EGAC
              | EGBB
              | EGCC
              | EGDB
              | EGDC
              | EGDG
              | EGDL
              | EGDM
              | EGDP
              | EGDR
              | EGDY
              | EGFF
              | EGHE
              | EGHH
              | EGHI
              | EGJB
              | EGJJ
              | EGKA
              | EGKK
              | EGLF
              | EGLL
              | EGNC
              | EGNH
              | EGNL
              | EGNR
              | EGNS
              | EGOM
              | EGOP
              | EGOS
              | EGOV
              | EGOY
              | EGPA
              | EGPB
              | EGPC
              | EGPD
              | EGPE
              | EGPF
              | EGPH
              | EGPK
              | EGPL
              | EGPO
              | EGPU
              | EGQA
              | EGQJ
              | EGQK
              | EGQL
              | EGQM
              | EGQS
              | EGRB
              | EGRR
              | EGSH
              | EGSS
              | EGTE
              | EGUB
              | EGUC
              | EGUF
              | EGUL
              | EGUM
              | EGUN
              | EGUW
              | EGUY
              | EGVA
              | EGVN
              | EGVO
              | EGVP
              | EGWU
              | EGWZ
              | EGXC
              | EGXD
              | EGXE
              | EGXG
              | EGXH
              | EGXJ
              | EGXT
              | EGXU
              | EGXV
              | EGXW
              | EGYC
              | EGYD
              | EGYH
              | EGYM
              | EGYP
              | EGYW
              | EHAM
              | EHBK
              | EHDB
              | EHDL
              | EHEH
              | EHGG
              | EHGR
              | EHKD
              | EHLE
              | EHLW
              | EHRD
              | EHSB
              | EHTW
              | EHVB
              | EHVK
              | EHVL
              | EHWO
              | EICK
              | EIDW
              | EIME
              | EINN
              | EKAH
              | EKAT
              | EKAV
              | EKBI
              | EKCH
              | EKEB
              | EKHO
              | EKHS
              | EKKA
              | EKMB
              | EKOD
              | EKRK
              | EKRN
              | EKSB
              | EKSN
              | EKSP
              | EKST
              | EKSV
              | EKTS
              | EKVA
              | EKVD
              | EKVG
              | EKVJ
              | EKVL
              | EKYT
              | ELLX
              | ENAL
              | ENAN
              | ENAS
              | ENAT
              | ENBJ
              | ENBM
              | ENBN
              | ENBO
              | ENBR
              | ENBV
              | ENCN
              | ENDU
              | ENEV
              | ENFB
              | ENFG
              | ENFL
              | ENGM
              | ENHD
              | ENHF
              | ENHO
              | ENHV
              | ENJA
              | ENKA
              | ENKB
              | ENKR
              | ENLI
              | ENLK
              | ENMH
              | ENML
              | ENMS
              | ENNA
              | ENNK
              | ENNM
              | ENOL
              | ENOV
              | ENRA
              | ENRM
              | ENRO
              | ENRS
              | ENRY
              | ENSB
              | ENSD
              | ENSG
              | ENSH
              | ENSK
              | ENSN
              | ENSR
              | ENSS
              | ENST
              | ENTC
              | ENTO
              | ENVA
              | ENVD
              | ENZV
              | EPGD
              | EPKK
              | EPKO
              | EPKT
              | EPPO
              | EPRZ
              | EPSC
              | EPWA
              | EPWR
              | EPZG
              | ESCF
              | ESCL
              | ESCM
              | ESDA
              | ESDB
              | ESDF
              | ESGG
              | ESGJ
              | ESGP
              | ESGR
              | ESIB
              | ESMK
              | ESMQ
              | ESMS
              | ESMT
              | ESMV
              | ESMX
              | ESNG
              | ESNJ
              | ESNK
              | ESNN
              | ESNO
              | ESNQ
              | ESNS
              | ESNU
              | ESOW
              | ESPA
              | ESPC
              | ESPD
              | ESPE
              | ESSA
              | ESSB
              | ESSD
              | ESSF
              | ESSK
              | ESSP
              | ESSQ
              | ESSV
              | ETAD
              | ETAR
              | ETAS
              | ETBA
              | ETEB
              | ETEH
              | ETEU
              | ETGB
              | ETGG
              | ETGI
              | ETGK
              | ETGW
              | ETGY
              | ETGZ
              | ETHA
              | ETHB
              | ETHC
              | ETHE
              | ETHF
              | ETHI
              | ETHL
              | ETHM
              | ETHN
              | ETHR
              | ETHS
              | ETHT
              | ETIC
              | ETID
              | ETIE
              | ETIH
              | ETIK
              | ETIN
              | ETME
              | ETMK
              | ETMN
              | ETND
              | ETNG
              | ETNH
              | ETNJ
              | ETNL
              | ETNN
              | ETNP
              | ETNR
              | ETNS
              | ETNT
              | ETNU
              | ETNW
              | ETOR
              | ETOU
              | ETSA
              | ETSB
              | ETSE
              | ETSF
              | ETSH
              | ETSI
              | ETSL
              | ETSM
              | ETSN
              | ETSP
              | ETUL
              | ETUN
              | ETUO
              | ETUR
              | EYVI
              | FAAB
              | FAAN
              | FABL
              | FABM
              | FABY
              | FACL
              | FACT
              | FACV
              | FADA
              | FADN
              | FADY
              | FAEL
              | FAER
              | FAFF
              | FAFR
              | FAGB
              | FAGE
              | FAGG
              | FAGR
              | FAHS
              | FAIR
              | FAJS
              | FAKD
              | FAKM
              | FALW
              | FALY
              | FAMB
              | FAME
              | FAMM
              | FAMO
              | FANS
              | FAOH
              | FAPB
              | FAPE
              | FAPH
              | FAPJ
              | FAPR
              | FAQT
              | FARB
              | FASB
              | FATC
              | FAUP
              | FAUT
              | FAVB
              | FAVR
              | FAWK
              | FAWM
              | FBFT
              | FBGZ
              | FBJW
              | FBKE
              | FBLT
              | FBMN
              | FBSK
              | FBSN
              | FBSW
              | FBTE
              | FBTS
              | FCBB
              | FCBD
              | FCBM
              | FCBO
              | FCBS
              | FCOG
              | FCOI
              | FCOM
              | FCOS
              | FCOU
              | FCPA
              | FCPL
              | FCPP
              | FDMS
              | FEFA
              | FEFB
              | FEFF
              | FEFG
              | FEFI
              | FEFL
              | FEFM
              | FEFN
              | FEFO
              | FEFR
              | FEFS
              | FEFT
              | FEFY
              | FGSL
              | FHAW
              | FIMP
              | FIMR
              | FJDG
              | FKAB
              | FKAF
              | FKAG
              | FKAL
              | FKAM
              | FKAN
              | FKAO
              | FKAY
              | FKKA
              | FKKB
              | FKKC
              | FKKD
              | FKKF
              | FKKI
              | FKKM
              | FKKN
              | FKKR
              | FKKV
              | FKYS
              | FLBA
              | FLCH
              | FLCP
              | FLIK
              | FLKB
              | FLKO
              | FLKS
              | FLKW
              | FLLC
              | FLLD
              | FLLI
              | FLLS
              | FLMA
              | FLMG
              | FLMP
              | FLMW
              | FLND
              | FLPA
              | FLPE
              | FLPO
              | FLSE
              | FLSN
              | FLSS
              | FLSW
              | FLZB
              | FMCH
              | FMCV
              | FMCZ
              | FMEE
              | FMEP
              | FMME
              | FMMH
              | FMMI
              | FMMO
              | FMMS
              | FMMT
              | FMMV
              | FMNA
              | FMND
              | FMNH
              | FMNL
              | FMNM
              | FMNN
              | FMNQ
              | FMNS
              | FMNV
              | FMSD
              | FMSF
              | FMSG
              | FMSM
              | FMSO
              | FMSR
              | FMST
              | FNBG
              | FNCA
              | FNHU
              | FNKU
              | FNLU
              | FOGM
              | FOGR
              | FOOB
              | FOOC
              | FOOD
              | FOOE
              | FOOG
              | FOOK
              | FOOL
              | FOOM
              | FOOR
              | FOOT
              | FOOY
              | FPPR
              | FPST
              | FQBR
              | FQCB
              | FQCH
              | FQIN
              | FQLC
              | FQLU
              | FQMA
              | FQMP
              | FQNP
              | FQPB
              | FQQL
              | FQTE
              | FQVL
              | FQXA
              | FSIA
              | FSSS
              | FTTA
              | FTTC
              | FTTD
              | FTTJ
              | FTTK
              | FTTL
              | FTTN
              | FTTP
              | FTTY
              | FVBB
              | FVBI
              | FVBU
              | FVCH
              | FVCZ
              | FVFA
              | FVGO
              | FVHA
              | FVKA
              | FVKB
              | FVMT
              | FVMV
              | FVRU
              | FVTL
              | FVWN
              | FWCL
              | FWCT
              | FWDZ
              | FWKA
              | FWKI
              | FWKK
              | FWMG
              | FWMY
              | FWMZ
              | FWSM
              | FWUU
              | FXMU
              | FYGF
              | FYKT
              | FYRU
              | FYWH
              | FZAA
              | FZAG
              | FZAM
              | FZAN
              | FZBA
              | FZBO
              | FZCA
              | FZCS
              | FZEA
              | FZFK
              | FZGN
              | FZIA
              | FZMB
              | FZNA
              | FZNC
              | FZOA
              | FZQA
              | FZQM
              | FZRA
              | FZRF
              | FZRQ
              | FZSA
              | FZUA
              | FZUK
              | FZVA
              | FZVI
              | FZVS
              | FZWA
              | GABG
              | GABS
              | GAGO
              | GAHB
              | GAKA
              | GAKL
              | GAKO
              | GAKT
              | GAKY
              | GAMB
              | GAMK
              | GANK
              | GANR
              | GASG
              | GASK
              | GASN
              | GATB
              | GATS
              | GBYD
              | GCFV
              | GCHI
              | GCLA
              | GCLP
              | GCRR
              | GCTS
              | GCXO
              | GEML
              | GFLL
              | GGBF
              | GGOV
              | GLRB
              | GMAA
              | GMAD
              | GMAT
              | GMFF
              | GMFI
              | GMFK
              | GMFM
              | GMFN
              | GMFO
              | GMFZ
              | GMMC
              | GMME
              | GMMF
              | GMMI
              | GMMN
              | GMMS
              | GMMX
              | GMMY
              | GMMZ
              | GMTA
              | GMTN
              | GMTT
              | GOGG
              | GOGK
              | GOGS
              | GOOD
              | GOOG
              | GOOK
              | GOOY
              | GOSM
              | GOSP
              | GOSS
              | GOTK
              | GOTT
              | GQNA
              | GQNB
              | GQND
              | GQNF
              | GQNI
              | GQNJ
              | GQNK
              | GQNN
              | GQNR
              | GQPA
              | GQPP
              | GQPT
              | GSVO
              | GUCY
              | GUFH
              | GUID
              | GUKU
              | GULB
              | GUMA
              | GUNZ
              | GUOK
              | GUSI
              | GUXD
              | GVAC
              | HAAB
              | HAAM
              | HABD
              | HADC
              | HADM
              | HADR
              | HAGN
              | HAGO
              | HAGR
              | HAHM
              | HAJJ
              | HAJM
              | HALA
              | HAMK
              | HAMS
              | HANG
              | HASB
              | HBBA
              | HCMH
              | HCMI
              | HCMM
              | HCMN
              | HCMV
              | HEAR
              | HEAT
              | HEAX
              | HECA
              | HEGN
              | HELX
              | HEMM
              | HEPS
              | HESN
              | HETR
              | HFFF
              | HHAS
              | HKEL
              | HKEM
              | HKGA
              | HKKG
              | HKKI
              | HKKR
              | HKKS
              | HKKT
              | HKLO
              | HKLU
              | HKMA
              | HKMB
              | HKME
              | HKML
              | HKMO
              | HKMU
              | HKMY
              | HKNA
              | HKNC
              | HKNI
              | HKNK
              | HKNO
              | HKNW
              | HKVO
              | HKWJ
              | HLGT
              | HLKF
              | HLLB
              | HLLS
              | HLLT
              | HLTD
              | HRYG
              | HRYR
              | HSAT
              | HSDN
              | HSDZ
              | HSFS
              | HSGF
              | HSGN
              | HSKA
              | HSKI
              | HSLI
              | HSNL
              | HSNR
              | HSOB
              | HSRN
              | HSSJ
              | HSSM
              | HSSP
              | HSSS
              | HSSW
              | HTAR
              | HTBU
              | HTDA
              | HTDO
              | HTIR
              | HTKA
              | HTKJ
              | HTMB
              | HTMG
              | HTMO
              | HTMS
              | HTMT
              | HTMU
              | HTMW
              | HTNA
              | HTPE
              | HTSE
              | HTSO
              | HTTB
              | HTTG
              | HTZA
              | HUAR
              | HUEN
              | HUGU
              | HUJI
              | HUKB
              | HUKS
              | HUMA
              | HUMI
              | HUSO
              | HUTO
              | K01R
              | K1K5
              | K1V4
              | K2C2
              | K2DP
              | K2PJ
              | K3RN
              | K44W
              | K4CB
              | K4MR
              | K4SU
              | K50Q
              | K87Q
              | K9B2
              | K9V9
              | KABE
              | KABI
              | KABQ
              | KABR
              | KABY
              | KACT
              | KACY
              | KADW
              | KAEX
              | KAFF
              | KAGR
              | KAGS
              | KAHN
              | KALB
              | KALO
              | KALS
              | KAMA
              | KANJ
              | KAPN
              | KAQQ
              | KAST
              | KATL
              | KATT
              | KAVL
              | KAVP
              | KAVX
              | KAYS
              | KBDL
              | KBDR
              | KBED
              | KBFF
              | KBFL
              | KBGM
              | KBHM
              | KBIH
              | KBIL
              | KBIS
              | KBJI
              | KBJN
              | KBKW
              | KBML
              | KBNA
              | KBNO
              | KBOI
              | KBOS
              | KBPI
              | KBPT
              | KBRO
              | KBTV
              | KBUF
              | KBUR
              | KBVE
              | KBWI
              | KBYS
              | KCAE
              | KCAG
              | KCAK
              | KCAO
              | KCAR
              | KCEF
              | KCHA
              | KCHH
              | KCHS
              | KCKL
              | KCLE
              | KCLT
              | KCMH
              | KCMX
              | KCNK
              | KCOD
              | KCOF
              | KCON
              | KCOS
              | KCOU
              | KCPR
              | KCRP
              | KCRW
              | KCTY
              | KCVG
              | KCYS
              | KD45
              | KDAY
              | KDBQ
              | KDCA
              | KDDC
              | KDEN
              | KDFW
              | KDGW
              | KDLH
              | KDNR
              | KDPG
              | KDRA
              | KDRT
              | KDSM
              | KDTW
              | KE28
              | KEDW
              | KEKA
              | KEKN
              | KELP
              | KELY
              | KENV
              | KEPO
              | KEPZ
              | KERI
              | KESC
              | KEUG
              | KEVV
              | KEWR
              | KEYW
              | KF10
              | KF30
              | KFAR
              | KFAT
              | KFBG
              | KFCS
              | KFFO
              | KFHU
              | KFMH
              | KFNT
              | KFOD
              | KFRI
              | KFSD
              | KFSI
              | KFSM
              | KFTK
              | KFWA
              | KFWD
              | KGBN
              | KGCC
              | KGDP
              | KGEG
              | KGGG
              | KGGW
              | KGJT
              | KGLD
              | KGLS
              | KGPI
              | KGRB
              | KGRF
              | KGRI
              | KGRR
              | KGSO
              | KGSP
              | KGTB
              | KGTF
              | KH92
              | KHAT
              | KHLN
              | KHLR
              | KHMN
              | KHMS
              | KHON
              | KHOP
              | KHRT
              | KHSV
              | KHTL
              | KHTS
              | KHVR
              | KIAD
              | KIAH
              | KICT
              | KIGM
              | KIND
              | KINL
              | KINS
              | KINW
              | KIPT
              | KISN
              | KJAN
              | KJAX
              | KJCT
              | KJFK
              | KLAN
              | KLAS
              | KLAX
              | KLBB
              | KLBF
              | KLCH
              | KLEX
              | KLFI
              | KLGA
              | KLGB
              | KLHW
              | KLIX
              | KLND
              | KLNK
              | KLRD
              | KLSE
              | KLSF
              | KLTS
              | KLWS
              | KLYH
              | KLZK
              | KMAF
              | KMCF
              | KMCI
              | KMCN
              | KMCO
              | KMDW
              | KMEI
              | KMEM
              | KMER
              | KMFR
              | KMGE
              | KMGM
              | KMHX
              | KMIA
              | KMKE
              | KMKG
              | KMLB
              | KMLI
              | KMLS
              | KMMO
              | KMOB
              | KMQT
              | KMRF
              | KMSN
              | KMSO
              | KMSP
              | KMSY
              | KMWN
              | KMWS
              | KMYR
              | KN78
              | KNEL
              | KNGZ
              | KNHK
              | KNHZ
              | KNID
              | KNJK
              | KNKT
              | KNKX
              | KNLC
              | KNOW
              | KNSI
              | KNTD
              | KNUQ
              | KO64
              | KOAK
              | KOAX
              | KOFF
              | KOFK
              | KOGD
              | KOKC
              | KOLM
              | KOMA
              | KONM
              | KORD
              | KORF
              | KOUN
              | KP07
              | KP11
              | KP24
              | KP28
              | KP35
              | KP38
              | KP39
              | KPAH
              | KPAM
              | KPBI
              | KPDT
              | KPDX
              | KPGA
              | KPHL
              | KPHX
              | KPIA
              | KPIH
              | KPIT
              | KPKF
              | KPMD
              | KPOB
              | KPOE
              | KPQI
              | KPTT
              | KPUB
              | KPUC
              | KPVD
              | KPWM
              | KQHA
              | KRAP
              | KRBL
              | KRDD
              | KRDU
              | KRFD
              | KRIC
              | KRIV
              | KRIW
              | KRKS
              | KRMG
              | KRNO
              | KROA
              | KROC
              | KROW
              | KRQE
              | KRST
              | KRUE
              | KRUM
              | KSAC
              | KSAN
              | KSAT
              | KSAV
              | KSBN
              | KSCK
              | KSDB
              | KSDF
              | KSEA
              | KSEP
              | KSFD
              | KSFO
              | KSGF
              | KSHR
              | KSHV
              | KSJT
              | KSLC
              | KSLE
              | KSLO
              | KSMX
              | KSNY
              | KSPS
              | KSSC
              | KSTJ
              | KSTL
              | KSUU
              | KSUX
              | KSVC
              | KSYR
              | KTBW
              | KTCM
              | KTCS
              | KTIK
              | KTLH
              | KTOL
              | KTOP
              | KTPA
              | KTUL
              | KTUP
              | KTUS
              | KTYS
              | KU67
              | KUIL
              | KVAD
              | KVBG
              | KVCT
              | KVPS
              | KVTN
              | KW30
              | KWAL
              | KWDD
              | KWMC
              | KWWR
              | KXMR
              | KYKM
              | KYNG
              | KYUM
              | LATI
              | LBBG
              | LBPD
              | LBRS
              | LBSF
              | LBWN
              | LCLK
              | LCNC
              | LCPH
              | LCRA
              | LDDD
              | LDDU
              | LDOR
              | LDOS
              | LDPL
              | LDRI
              | LDSH
              | LDSP
              | LDVA
              | LDZA
              | LDZD
              | LEAB
              | LEAL
              | LEAM
              | LEAS
              | LEBA
              | LEBB
              | LEBG
              | LEBL
              | LEBZ
              | LECH
              | LECO
              | LECV
              | LEGE
              | LEGR
              | LEGT
              | LEHI
              | LEIB
              | LEJR
              | LELC
              | LELN
              | LELO
              | LEMD
              | LEMG
              | LEMH
              | LEMO
              | LEPA
              | LEPP
              | LERI
              | LERS
              | LERT
              | LESA
              | LESO
              | LEST
              | LETO
              | LEVC
              | LEVD
              | LEVS
              | LEVT
              | LEVX
              | LEXJ
              | LEZG
              | LEZL
              | LFAT
              | LFBA
              | LFBC
              | LFBD
              | LFBE
              | LFBF
              | LFBG
              | LFBH
              | LFBI
              | LFBL
              | LFBM
              | LFBN
              | LFBO
              | LFBP
              | LFBS
              | LFBT
              | LFBV
              | LFBX
              | LFBY
              | LFBZ
              | LFCG
              | LFCI
              | LFCR
              | LFDH
              | LFHP
              | LFIG
              | LFJL
              | LFKB
              | LFKC
              | LFKF
              | LFKJ
              | LFKS
              | LFLA
              | LFLB
              | LFLC
              | LFLD
              | LFLL
              | LFLM
              | LFLN
              | LFLQ
              | LFLS
              | LFLV
              | LFLW
              | LFLX
              | LFLY
              | LFMA
              | LFMC
              | LFMD
              | LFME
              | LFMH
              | LFMI
              | LFMK
              | LFML
              | LFMN
              | LFMO
              | LFMP
              | LFMT
              | LFMX
              | LFMY
              | LFOB
              | LFOC
              | LFOE
              | LFOF
              | LFOH
              | LFOI
              | LFOJ
              | LFOP
              | LFOR
              | LFOS
              | LFOT
              | LFOW
              | LFPB
              | LFPC
              | LFPG
              | LFPM
              | LFPN
              | LFPO
              | LFPV
              | LFQB
              | LFQG
              | LFQH
              | LFQI
              | LFQQ
              | LFQV
              | LFRA
              | LFRB
              | LFRC
              | LFRD
              | LFRH
              | LFRI
              | LFRJ
              | LFRK
              | LFRL
              | LFRM
              | LFRN
              | LFRQ
              | LFRS
              | LFRT
              | LFRZ
              | LFSA
              | LFSB
              | LFSC
              | LFSD
              | LFSF
              | LFSI
              | LFSL
              | LFSN
              | LFSO
              | LFSQ
              | LFSR
              | LFST
              | LFSX
              | LFTH
              | LFTU
              | LFTW
              | LFVP
              | LFXA
              | LFYR
              | LGAD
              | LGAL
              | LGAT
              | LGBL
              | LGEL
              | LGHI
              | LGIR
              | LGKA
              | LGKF
              | LGKL
              | LGKO
              | LGKP
              | LGKR
              | LGKV
              | LGKZ
              | LGLM
              | LGLR
              | LGMT
              | LGPZ
              | LGRP
              | LGRX
              | LGSA
              | LGSM
              | LGTG
              | LGTP
              | LGTS
              | LGTT
              | LHBC
              | LHBM
              | LHBP
              | LHBS
              | LHDC
              | LHKE
              | LHKV
              | LHMC
              | LHNY
              | LHPA
              | LHPP
              | LHSK
              | LHSN
              | LHSY
              | LHUD
              | LIBA
              | LIBC
              | LIBD
              | LIBE
              | LIBG
              | LIBH
              | LIBN
              | LIBP
              | LIBQ
              | LIBR
              | LIBS
              | LIBT
              | LIBU
              | LIBV
              | LIBW
              | LIBY
              | LIBZ
              | LICA
              | LICC
              | LICD
              | LICE
              | LICF
              | LICG
              | LICJ
              | LICL
              | LICO
              | LICP
              | LICR
              | LICT
              | LICU
              | LICZ
              | LIEA
              | LIEB
              | LIEC
              | LIED
              | LIEE
              | LIEF
              | LIEG
              | LIEH
              | LIEL
              | LIEN
              | LIEO
              | LIEP
              | LIMC
              | LIME
              | LIMF
              | LIMG
              | LIMH
              | LIMJ
              | LIMK
              | LIML
              | LIMN
              | LIMO
              | LIMS
              | LIMT
              | LIMU
              | LIMV
              | LIMY
              | LIPA
              | LIPB
              | LIPC
              | LIPD
              | LIPE
              | LIPF
              | LIPH
              | LIPI
              | LIPK
              | LIPL
              | LIPQ
              | LIPR
              | LIPS
              | LIPT
              | LIPX
              | LIPY
              | LIPZ
              | LIQB
              | LIQC
              | LIQJ
              | LIQK
              | LIQN
              | LIQO
              | LIQP
              | LIQR
              | LIQV
              | LIQW
              | LIQZ
              | LIRA
              | LIRB
              | LIRE
              | LIRF
              | LIRG
              | LIRH
              | LIRJ
              | LIRK
              | LIRL
              | LIRM
              | LIRN
              | LIRP
              | LIRQ
              | LIRS
              | LIRT
              | LIRU
              | LIRV
              | LIRZ
              | LIVC
              | LIVD
              | LIVF
              | LIVM
              | LIVO
              | LIVP
              | LIVR
              | LIVT
              | LIYW
              | LJLJ
              | LJMB
              | LJMS
              | LJPZ
              | LKHO
              | LKKV
              | LKKZ
              | LKMT
              | LKPP
              | LKPR
              | LKSL
              | LKTB
              | LLBG
              | LLBS
              | LLET
              | LLHA
              | LLJR
              | LLOV
              | LMML
              | LOAV
              | LOWG
              | LOWI
              | LOWK
              | LOWL
              | LOWS
              | LOWW
              | LOXA
              | LOXL
              | LOXS
              | LOXT
              | LOXZ
              | LPAZ
              | LPBG
              | LPBJ
              | LPFL
              | LPFR
              | LPFU
              | LPHR
              | LPLA
              | LPPD
              | LPPR
              | LPPS
              | LPPT
              | LPVR
              | LQBI
              | LQBK
              | LQLV
              | LQMO
              | LQSA
              | LQTZ
              | LRAR
              | LRBC
              | LRBM
              | LRBS
              | LRCK
              | LRCL
              | LRCS
              | LRCV
              | LRIA
              | LROD
              | LROP
              | LRSB
              | LRSM
              | LRSV
              | LRTC
              | LRTM
              | LRTR
              | LSGG
              | LSGL
              | LSGN
              | LSGS
              | LSMP
              | LSZA
              | LSZG
              | LSZH
              | LSZR
              | LTAC
              | LTAD
              | LTAE
              | LTAF
              | LTAG
              | LTAH
              | LTAI
              | LTAJ
              | LTAK
              | LTAN
              | LTAP
              | LTAQ
              | LTAR
              | LTAS
              | LTAT
              | LTAU
              | LTAW
              | LTBA
              | LTBD
              | LTBE
              | LTBF
              | LTBG
              | LTBH
              | LTBI
              | LTBJ
              | LTBL
              | LTBM
              | LTBO
              | LTBS
              | LTBT
              | LTBV
              | LTCA
              | LTCC
              | LTCD
              | LTCE
              | LTCF
              | LTCG
              | LTCH
              | LTCI
              | LTCJ
              | LWOH
              | LWSK
              | LXGB
              | LYBE
              | LYNI
              | LYPR
              | LYPZ
              | LYTI
              | LYTV
              | LYVR
              | LZIB
              | LZKC
              | LZLU
              | LZTT
              | MDBH
              | MDHE
              | MDPC
              | MDPP
              | MDSD
              | MDST
              | MGCB
              | MGFL
              | MGGT
              | MGHT
              | MGPB
              | MGRT
              | MGSJ
              | MHAM
              | MHCA
              | MHCH
              | MHIC
              | MHLC
              | MHLE
              | MHLM
              | MHNO
              | MHPL
              | MHRO
              | MHSC
              | MHSR
              | MHTE
              | MHTG
              | MHYR
              | MKJP
              | MKJS
              | MMAN
              | MMAS
              | MMCB
              | MMCL
              | MMCM
              | MMCN
              | MMCP
              | MMEP
              | MMHO
              | MMIA
              | MMIO
              | MMLT
              | MMMD
              | MMMV
              | MMPB
              | MMPG
              | MMQT
              | MMSP
              | MMTC
              | MMTG
              | MMTL
              | MNBL
              | MNCH
              | MNJG
              | MNJU
              | MNMG
              | MNPC
              | MNRS
              | MPDA
              | MPFS
              | MPHO
              | MPSA
              | MPTO
              | MRLB
              | MRLM
              | MRNC
              | MROC
              | MRPM
              | MSAC
              | MSLP
              | MSSA
              | MSSM
              | MSSS
              | MTCH
              | MUBA
              | MUCA
              | MUCF
              | MUCM
              | MUCU
              | MUGM
              | MUGT
              | MUHA
              | MUMZ
              | MUNG
              | MUPR
              | MUVR
              | MUVT
              | MWCR
              | MYBS
              | MYEG
              | MYGF
              | MYGW
              | MYIG
              | MYNN
              | MYSM
              | MZBZ
              | NCRG
              | NFFN
              | NFNA
              | NFNK
              | NFNR
              | NFTF
              | NFTL
              | NGBR
              | NGFO
              | NGFU
              | NGTA
              | NGTR
              | NGTU
              | NIUE
              | NLWF
              | NLWW
              | NSAP
              | NSFA
              | NSTU
              | NTAA
              | NTAT
              | NTTB
              | NTTG
              | NTTO
              | NTTX
              | NVSC
              | NVSL
              | NVSS
              | NVVV
              | NWWE
              | NWWK
              | NWWL
              | NWWN
              | NWWR
              | NWWV
              | NWWW
              | NZAA
              | NZCH
              | NZCI
              | NZCM
              | NZDN
              | NZGS
              | NZHK
              | NZKI
              | NZNP
              | NZNV
              | NZOH
              | NZPP
              | NZRN
              | NZRO
              | NZSP
              | NZTG
              | NZWN
              | NZWP
              | OAFR
              | OAFZ
              | OAHR
              | OAJL
              | OAJS
              | OAKB
              | OAMS
              | OASD
              | OASG
              | OAZB
              | OAZG
              | OBBI
              | OEAB
              | OEAH
              | OEBA
              | OEBH
              | OEDF
              | OEDR
              | OEDW
              | OEGN
              | OEGS
              | OEGT
              | OEHL
              | OEJD
              | OEJN
              | OEKK
              | OEKM
              | OEMA
              | OEMK
              | OENG
              | OEPA
              | OERF
              | OERK
              | OERR
              | OERY
              | OESH
              | OESK
              | OETB
              | OETF
              | OETR
              | OEWD
              | OEWJ
              | OEYN
              | OIAA
              | OIAG
              | OIAH
              | OIAI
              | OIAW
              | OIBB
              | OIBL
              | OICC
              | OICG
              | OICS
              | OIFK
              | OIFM
              | OIFS
              | OIGG
              | OIHR
              | OIII
              | OIIK
              | OIIS
              | OIKB
              | OIKK
              | OIKM
              | OIMB
              | OIMH
              | OIMM
              | OIMN
              | OIMS
              | OIMT
              | OING
              | OINR
              | OISA
              | OISF
              | OISS
              | OITK
              | OITR
              | OITS
              | OITT
              | OITZ
              | OIYY
              | OIZB
              | OIZC
              | OIZH
              | OIZI
              | OIZJ
              | OJAI
              | OJAM
              | OJAQ
              | OJBD
              | OJHF
              | OJHR
              | OJMF
              | OJMN
              | OKBK
              | OLBA
              | OMAA
              | OMAD
              | OMAL
              | OMDB
              | OMFJ
              | OMRK
              | OMSJ
              | OOBR
              | OOFD
              | OOKB
              | OOMA
              | OOMS
              | OOSA
              | OOSH
              | OOSQ
              | OOSR
              | OOTH
              | OPDI
              | OPJA
              | OPJI
              | OPKC
              | OPKD
              | OPLA
              | OPLH
              | OPMI
              | OPMT
              | OPNH
              | OPPG
              | OPPS
              | OPQT
              | OPRN
              | OPRS
              | OPSB
              | OPSR
              | ORBB
              | ORBM
              | ORMS
              | OSAP
              | OSDI
              | OSDZ
              | OSKL
              | OSLK
              | OSPR
              | OTBD
              | OYAA
              | OYAR
              | OYAT
              | OYHD
              | OYMB
              | OYMC
              | OYMK
              | OYSN
              | OYSQ
              | OYTZ
              | OYZM
              | PAAQ
              | PABA
              | PABE
              | PABI
              | PABL
              | PABR
              | PABT
              | PACD
              | PACP
              | PACV
              | PACY
              | PACZ
              | PADK
              | PADL
              | PADQ
              | PADU
              | PAED
              | PAEH
              | PAEI
              | PAEN
              | PAFA
              | PAFM
              | PAFR
              | PAGA
              | PAGK
              | PAGM
              | PAGN
              | PAGS
              | PAGY
              | PAHD
              | PAIL
              | PAIM
              | PAJN
              | PAKN
              | PAKO
              | PAKT
              | PAKW
              | PALU
              | PAMC
              | PAMD
              | PAMH
              | PANC
              | PANI
              | PANN
              | PANT
              | PAOM
              | PAOR
              | PAOT
              | PAPC
              | PAPG
              | PAPH
              | PAPT
              | PAQT
              | PASI
              | PASN
              | PASV
              | PASW
              | PASY
              | PATA
              | PATC
              | PATK
              | PAUM
              | PAUN
              | PAWD
              | PAWG
              | PAWI
              | PAYA
              | PFYU
              | PGAC
              | PGRO
              | PGSN
              | PGUA
              | PGUM
              | PGWT
              | PHBK
              | PHHI
              | PHJR
              | PHLI
              | PHMK
              | PHNG
              | PHNL
              | PHOG
              | PHTO
              | PHWH
              | PJON
              | PKMJ
              | PKWA
              | PLCH
              | PLFA
              | PMDY
              | POLI
              | PPIZ
              | PTKK
              | PTPN
              | PTRO
              | PTSA
              | PTTK
              | PTYA
              | PWAK
              | RCAY
              | RCBS
              | RCDC
              | RCFG
              | RCFS
              | RCGM
              | RCKH
              | RCKU
              | RCKW
              | RCLG
              | RCLM
              | RCLY
              | RCMJ
              | RCMQ
              | RCMS
              | RCNN
              | RCNO
              | RCPO
              | RCQC
              | RCQS
              | RCSQ
              | RCSS
              | RCTP
              | RCUK
              | RCYU
              | RJAA
              | RJAF
              | RJAH
              | RJAK
              | RJAM
              | RJAO
              | RJAT
              | RJAW
              | RJBB
              | RJBD
              | RJCA
              | RJCB
              | RJCC
              | RJCH
              | RJCJ
              | RJCK
              | RJCM
              | RJCN
              | RJCO
              | RJCS
              | RJCW
              | RJCY
              | RJDB
              | RJDC
              | RJDM
              | RJDT
              | RJEB
              | RJEC
              | RJFA
              | RJFC
              | RJFE
              | RJFF
              | RJFG
              | RJFK
              | RJFM
              | RJFN
              | RJFO
              | RJFR
              | RJFT
              | RJFU
              | RJFW
              | RJFY
              | RJFZ
              | RJKA
              | RJKB
              | RJKN
              | RJNF
              | RJNG
              | RJNH
              | RJNK
              | RJNN
              | RJNO
              | RJNT
              | RJNY
              | RJOA
              | RJOB
              | RJOC
              | RJOE
              | RJOF
              | RJOH
              | RJOI
              | RJOK
              | RJOM
              | RJOO
              | RJOP
              | RJOR
              | RJOS
              | RJOT
              | RJOW
              | RJOY
              | RJOZ
              | RJSA
              | RJSC
              | RJSD
              | RJSF
              | RJSH
              | RJSI
              | RJSK
              | RJSM
              | RJSN
              | RJSO
              | RJSS
              | RJST
              | RJSU
              | RJTA
              | RJTC
              | RJTD
              | RJTE
              | RJTF
              | RJTH
              | RJTI
              | RJTJ
              | RJTK
              | RJTL
              | RJTO
              | RJTQ
              | RJTR
              | RJTT
              | RJTU
              | RJTX
              | RJTY
              | RKJJ
              | RKJK
              | RKJY
              | RKNC
              | RKNH
              | RKNN
              | RKNW
              | RKPC
              | RKPK
              | RKPM
              | RKPS
              | RKPU
              | RKSB
              | RKSF
              | RKSG
              | RKSI
              | RKSL
              | RKSM
              | RKSN
              | RKSO
              | RKSP
              | RKSS
              | RKSW
              | RKTA
              | RKTD
              | RKTE
              | RKTF
              | RKTH
              | RKTI
              | RKTM
              | RKTN
              | RKTT
              | RKTU
              | RKTY
              | ROAH
              | RODE
              | RODN
              | ROHF
              | ROIG
              | ROKJ
              | ROMD
              | ROMY
              | ROTM
              | ROYN
              | RPLB
              | RPLI
              | RPLL
              | RPMD
              | RPMK
              | RPMP
              | RPMR
              | RPMS
              | RPMT
              | RPMZ
              | RPUA
              | RPUB
              | RPUD
              | RPUH
              | RPUI
              | RPUK
              | RPUO
              | RPUQ
              | RPUR
              | RPUT
              | RPUV
              | RPVA
              | RPVD
              | RPVF
              | RPVG
              | RPVI
              | RPVM
              | RPVP
              | RPVR
              | RPVT
              | RPWB
              | RPWC
              | RPWE
              | RPWG
              | RPWL
              | RPWS
              | RPWY
              | RPXT
              | SAAC
              | SAAG
              | SAAJ
              | SAAP
              | SAAR
              | SAAU
              | SAAV
              | SABA
              | SABE
              | SACI
              | SACO
              | SACP
              | SACV
              | SADD
              | SADL
              | SADP
              | SAEZ
              | SAME
              | SAMI
              | SAMJ
              | SAMM
              | SAMR
              | SAMS
              | SAMU
              | SANC
              | SANE
              | SANI
              | SANL
              | SANO
              | SANT
              | SANU
              | SANW
              | SAOC
              | SAOD
              | SAOL
              | SAOM
              | SAOR
              | SAOU
              | SARC
              | SARE
              | SARF
              | SARI
              | SARL
              | SARM
              | SARP
              | SARS
              | SASA
              | SASJ
              | SASO
              | SASQ
              | SASR
              | SAST
              | SATK
              | SATU
              | SAVB
              | SAVC
              | SAVE
              | SAVO
              | SAVP
              | SAVT
              | SAVV
              | SAWA
              | SAWD
              | SAWE
              | SAWG
              | SAWH
              | SAWJ
              | SAWP
              | SAWR
              | SAWU
              | SAZA
              | SAZB
              | SAZD
              | SAZE
              | SAZG
              | SAZH
              | SAZM
              | SAZN
              | SAZP
              | SAZQ
              | SAZR
              | SAZS
              | SAZT
              | SAZV
              | SAZY
              | SBAA
              | SBAF
              | SBAN
              | SBAR
              | SBAT
              | SBBC
              | SBBE
              | SBBG
              | SBBH
              | SBBI
              | SBBQ
              | SBBR
              | SBBU
              | SBBV
              | SBBW
              | SBCF
              | SBCG
              | SBCI
              | SBCO
              | SBCP
              | SBCR
              | SBCT
              | SBCV
              | SBCY
              | SBCZ
              | SBDN
              | SBEG
              | SBEK
              | SBES
              | SBFI
              | SBFL
              | SBFN
              | SBFZ
              | SBGA
              | SBGL
              | SBGO
              | SBGW
              | SBHT
              | SBIH
              | SBIL
              | SBIZ
              | SBJF
              | SBJP
              | SBJR
              | SBKG
              | SBKP
              | SBLO
              | SBLP
              | SBMA
              | SBMG
              | SBMK
              | SBMN
              | SBMO
              | SBMQ
              | SBMS
              | SBMT
              | SBMY
              | SBNT
              | SBOI
              | SBPA
              | SBPB
              | SBPC
              | SBPF
              | SBPG
              | SBPK
              | SBPL
              | SBPN
              | SBPP
              | SBPV
              | SBQV
              | SBRB
              | SBRF
              | SBRJ
              | SBRS
              | SBSA
              | SBSC
              | SBSL
              | SBSM
              | SBSN
              | SBSP
              | SBST
              | SBSV
              | SBTE
              | SBTF
              | SBTK
              | SBTT
              | SBTU
              | SBUA
              | SBUF
              | SBUG
              | SBUR
              | SBVH
              | SBVT
              | SBXV
              | SBYA
              | SBYS
              | SCAR
              | SCBA
              | SCCC
              | SCCH
              | SCCI
              | SCCY
              | SCDA
              | SCEL
              | SCER
              | SCFA
              | SCHA
              | SCHR
              | SCIC
              | SCIE
              | SCIP
              | SCLL
              | SCRA
              | SCSE
              | SCTC
              | SCTE
              | SCVD
              | SEAM
              | SEBC
              | SECU
              | SEES
              | SEGU
              | SEIB
              | SELO
              | SELT
              | SEMA
              | SEMH
              | SEMT
              | SEPA
              | SEQU
              | SESA
              | SEST
              | SETI
              | SETU
              | SFAL
              | SGAS
              | SGCO
              | SGEN
              | SGME
              | SGNA
              | SKAR
              | SKAS
              | SKBG
              | SKBO
              | SKBQ
              | SKBU
              | SKCC
              | SKCG
              | SKCL
              | SKEJ
              | SKIB
              | SKIP
              | SKLC
              | SKLT
              | SKMD
              | SKMR
              | SKMU
              | SKNV
              | SKPC
              | SKPE
              | SKPP
              | SKPS
              | SKPV
              | SKRG
              | SKRH
              | SKSJ
              | SKSM
              | SKSP
              | SKUC
              | SKUI
              | SKVP
              | SKVV
              | SLAP
              | SLAS
              | SLCA
              | SLCB
              | SLCN
              | SLCO
              | SLCP
              | SLET
              | SLGY
              | SLJE
              | SLJO
              | SLJV
              | SLLP
              | SLMG
              | SLOR
              | SLPO
              | SLPS
              | SLRB
              | SLRI
              | SLRQ
              | SLRY
              | SLSA
              | SLSB
              | SLSI
              | SLSM
              | SLSU
              | SLTJ
              | SLTR
              | SLVM
              | SLVR
              | SLYA
              | SMZY
              | SOCA
              | SOOM
              | SPAY
              | SPCL
              | SPEO
              | SPGM
              | SPHI
              | SPHO
              | SPHY
              | SPHZ
              | SPIM
              | SPJA
              | SPJI
              | SPJL
              | SPJN
              | SPJR
              | SPME
              | SPMS
              | SPNC
              | SPPY
              | SPQT
              | SPQU
              | SPRU
              | SPSO
              | SPST
              | SPTN
              | SPTU
              | SPUR
              | SPYL
              | SPZO
              | SUAA
              | SUAG
              | SUCA
              | SUDU
              | SUME
              | SUMO
              | SUMU
              | SUPU
              | SURV
              | SUSO
              | SUTB
              | SUTR
              | SVAC
              | SVBC
              | SVBI
              | SVBM
              | SVBS
              | SVCB
              | SVCL
              | SVCR
              | SVCU
              | SVFM
              | SVGD
              | SVGI
              | SVGU
              | SVJM
              | SVLO
              | SVMC
              | SVMD
              | SVMI
              | SVMN
              | SVMT
              | SVPA
              | SVPC
              | SVSA
              | SVSE
              | SVSO
              | SVSR
              | SVTM
              | SVTR
              | SVVA
              | SVVL
              | SVVP
              | SWBC
              | SYGT
              | SYTM
              | TAPA
              | TBPB
              | TDCF
              | TDPD
              | TDPR
              | TFFF
              | TFFJ
              | TFFR
              | TGPY
              | TIST
              | TISX
              | TJBQ
              | TJNR
              | TKPK
              | TKPN
              | TLPC
              | TLPL
              | TNCA
              | TNCB
              | TNCC
              | TNCE
              | TNCM
              | TTPP
              | TTPT
              | TUPJ
              | TVSV
              | TXKF
              | UAAA
              | UAII
              | UAKK
              | UAOO
              | UARR
              | UATA
              | UATT
              | UBBB
              | UEEE
              | UELL
              | UGEE
              | UGGG
              | UGMM
              | UHBP
              | UHHH
              | UHHO
              | UHMA
              | UHMD
              | UHMM
              | UHPP
              | UHSS
              | UHWW
              | UIAA
              | UIII
              | UIIO
              | UIKB
              | UIKK
              | UINN
              | UIUH
              | UIUU
              | UKBB
              | UKFF
              | UKHH
              | UKII
              | UKKK
              | UKLL
              | UKLR
              | UKOO
              | ULAA
              | ULAK
              | ULLI
              | ULMM
              | ULOL
              | ULWT
              | ULWW
              | UMII
              | UMMS
              | UMRR
              | UMRW
              | UNBB
              | UNII
              | UNNN
              | URKK
              | URMM
              | URRR
              | URSS
              | URWI
              | URWW
              | USHH
              | USRR
              | USSS
              | UTDD
              | UTED
              | UTSM
              | UTSS
              | UTST
              | UTTT
              | UUBP
              | UUEM
              | UUOO
              | UUYT
              | UUYY
              | UWPP
              | UWWW
              | VAAH
              | VAAK
              | VAAU
              | VABB
              | VABI
              | VABJ
              | VABM
              | VABP
              | VABV
              | VAGO
              | VAID
              | VAJB
              | VAKD
              | VAKP
              | VANP
              | VARK
              | VASL
              | VBBM
              | VBBS
              | VBCI
              | VBHL
              | VBKG
              | VBKP
              | VBLS
              | VBMK
              | VBML
              | VBMM
              | VBPA
              | VBPR
              | VBPT
              | VBRM
              | VBRN
              | VBRR
              | VBSY
              | VBTV
              | VCBI
              | VCCA
              | VCCB
              | VCCC
              | VCCT
              | VDKC
              | VDPP
              | VDSR
              | VEAT
              | VEBD
              | VEBS
              | VECC
              | VECX
              | VEGK
              | VEGT
              | VEGY
              | VEIM
              | VEJH
              | VEJS
              | VELR
              | VEMN
              | VEPB
              | VEPT
              | VERC
              | VGCB
              | VGEG
              | VGIS
              | VGJR
              | VGRJ
              | VGSY
              | VGTJ
              | VGZR
              | VHCH
              | VHHH
              | VIAG
              | VIAL
              | VIAR
              | VIBN
              | VIBY
              | VICX
              | VIDD
              | VIDP
              | VIGR
              | VIHR
              | VIJN
              | VIJO
              | VIJP
              | VIKO
              | VILK
              | VIST
              | VIUD
              | VLAP
              | VLIP
              | VLLB
              | VLSB
              | VLSK
              | VLSV
              | VLTK
              | VLVT
              | VMMC
              | VNBW
              | VNJL
              | VNKT
              | VNPK
              | VNSI
              | VNSK
              | VNTJ
              | VNVT
              | VOBI
              | VOBZ
              | VOCB
              | VOCC
              | VOCP
              | VOHY
              | VOMD
              | VOML
              | VOMM
              | VOTR
              | VOTV
              | VOVR
              | VRGN
              | VRMM
              | VTBC
              | VTBD
              | VTBG
              | VTBI
              | VTBJ
              | VTBP
              | VTBS
              | VTBU
              | VTCC
              | VTCH
              | VTCL
              | VTCN
              | VTCP
              | VTCR
              | VTCS
              | VTPH
              | VTPM
              | VTPN
              | VTPS
              | VTPT
              | VTPU
              | VTSA
              | VTSB
              | VTSD
              | VTSH
              | VTSK
              | VTSN
              | VTSP
              | VTSR
              | VTSS
              | VTST
              | VTUB
              | VTUC
              | VTUD
              | VTUK
              | VTUL
              | VTUM
              | VTUN
              | VTUP
              | VTUR
              | VTUS
              | VTUU
              | VVDN
              | VVNB
              | VVNT
              | VVPB
              | VVPK
              | VVQN
              | VVTS
              | VVVH
              | VYSW
              | VYYY
              | WAAA
              | WAAB
              | WAAU
              | WABB
              | WABI
              | WABN
              | WABO
              | WABT
              | WAJI
              | WAJJ
              | WAJW
              | WAKK
              | WAKT
              | WAMA
              | WAMG
              | WAMH
              | WAMI
              | WAML
              | WAMM
              | WAMP
              | WAMT
              | WAMW
              | WAPA
              | WAPH
              | WAPI
              | WAPN
              | WAPP
              | WAPR
              | WASF
              | WASK
              | WASR
              | WASS
              | WBGB
              | WBGG
              | WBGR
              | WBGS
              | WBKK
              | WBKL
              | WBKS
              | WBKT
              | WBKW
              | WBSB
              | WIAA
              | WIAG
              | WIAM
              | WIAR
              | WIAS
              | WIBB
              | WIIA
              | WIIB
              | WIIH
              | WIII
              | WIIJ
              | WIIK
              | WIIL
              | WIIS
              | WIIT
              | WIKB
              | WIKD
              | WIKK
              | WIKN
              | WIKS
              | WIMB
              | WIMG
              | WIMM
              | WIMS
              | WIOI
              | WIOK
              | WION
              | WIOO
              | WIOS
              | WIPA
              | WIPH
              | WIPL
              | WIPP
              | WIPR
              | WITC
              | WITM
              | WITT
              | WMBA
              | WMKC
              | WMKD
              | WMKJ
              | WMKK
              | WMKL
              | WMKM
              | WMKP
              | WPDL
              | WPEC
              | WPOC
              | WRBB
              | WRBI
              | WRBK
              | WRBM
              | WRBP
              | WRKC
              | WRKK
              | WRKL
              | WRKM
              | WRKR
              | WRKS
              | WRLB
              | WRLG
              | WRLK
              | WRLL
              | WRLR
              | WRLS
              | WRRA
              | WRRB
              | WRRR
              | WRRS
              | WRRW
              | WRSJ
              | WRSP
              | WRSQ
              | WRSS
              | WSAP
              | WSSS
              | YBAF
              | YBAM
              | YBAS
              | YBBN
              | YBCG
              | YBCS
              | YBCV
              | YBGL
              | YBLR
              | YBMA
              | YBMK
              | YBOK
              | YBPN
              | YBRK
              | YBRM
              | YBTL
              | YBWP
              | YDGV
              | YDTC
              | YDYL
              | YMAY
              | YMDV
              | YMEN
              | YMES
              | YMHB
              | YMLT
              | YMLV
              | YMMB
              | YMMG
              | YMMI
              | YMML
              | YMMQ
              | YMWY
              | YPAD
              | YPAL
              | YPBH
              | YPCC
              | YPCD
              | YPDB
              | YPDN
              | YPEA
              | YPED
              | YPFT
              | YPGN
              | YPKG
              | YPKU
              | YPLC
              | YPLM
              | YPMR
              | YPPD
              | YPPF
              | YPPH
              | YPWR
              | YPXM
              | YSBK
              | YSCB
              | YSCH
              | YSCM
              | YSCN
              | YSDU
              | YSNF
              | YSNW
              | YSRI
              | YSSY
              | YSTW
              | YSWG
              | YSWM
              | ZBAA
              | ZBHH
              | ZBYN
              | ZGCS
              | ZGGG
              | ZGHK
              | ZGKL
              | ZGNN
              | ZGOW
              | ZGSZ
              | ZGZJ
              | ZHCC
              | ZHHH
              | ZKKC
              | ZKPY
              | ZLIC
              | ZLJQ
              | ZLSN
              | ZLXN
              | ZLYA
              | ZPPP
              | ZSAM
              | ZSCN
              | ZSFZ
              | ZSGZ
              | ZSHC
              | ZSNJ
              | ZSOF
              | ZSQD
              | ZSSS
              | ZSTN
              | ZUCK
              | ZUGY
              | ZULS
              | ZUUU
              | ZWHM
              | ZWSH
              | ZWTN
              | ZWYN
              | ZYCC
              | ZYQQ
              | ZYTL
          deriving (Bounded, Enum, Eq, Ord, Read, Show)

data ICAOData = ICAOData { icaoPoint :: Point }

icaoData = Map.fromDistinctAscList [
    (AGGH, ICAOData (Point {pntLat = -9.416666666666666, pntLon = 160.05, pntEle = Just 8.0, pntTime = Nothing}))
  , (AGGL, ICAOData (Point {pntLat = -10.7, pntLon = 165.8, pntEle = Just 23.0, pntTime = Nothing}))
  , (AGGM, ICAOData (Point {pntLat = -8.333333333333334, pntLon = 157.26666666666668, pntEle = Just 6.0, pntTime = Nothing}))
  , (ANAU, ICAOData (Point {pntLat = -0.5333333333333333, pntLon = 166.91666666666666, pntEle = Just 6.0, pntTime = Nothing}))
  , (AYMD, ICAOData (Point {pntLat = -5.216666666666667, pntLon = 145.78333333333333, pntEle = Just 3.0, pntTime = Nothing}))
  , (AYPY, ICAOData (Point {pntLat = -9.433333333333334, pntLon = 147.21666666666667, pntEle = Just 38.0, pntTime = Nothing}))
  , (AYWK, ICAOData (Point {pntLat = -3.5666666666666664, pntLon = 143.63333333333333, pntEle = Just 6.0, pntTime = Nothing}))
  , (BGAM, ICAOData (Point {pntLat = 65.6, pntLon = -37.63333333333333, pntEle = Just 50.0, pntTime = Nothing}))
  , (BGAS, ICAOData (Point {pntLat = 59.983333333333334, pntLon = -45.2, pntEle = Just 16.0, pntTime = Nothing}))
  , (BGAT, ICAOData (Point {pntLat = 67.78333333333333, pntLon = -32.3, pntEle = Just 13.0, pntTime = Nothing}))
  , (BGBW, ICAOData (Point {pntLat = 61.166666666666664, pntLon = -45.416666666666664, pntEle = Just 34.0, pntTime = Nothing}))
  , (BGCO, ICAOData (Point {pntLat = 70.75, pntLon = -22.65, pntEle = Just 14.0, pntTime = Nothing}))
  , (BGDB, ICAOData (Point {pntLat = 74.3, pntLon = -20.216666666666665, pntEle = Just 44.0, pntTime = Nothing}))
  , (BGDH, ICAOData (Point {pntLat = 76.76666666666667, pntLon = -18.666666666666668, pntEle = Just 11.0, pntTime = Nothing}))
  , (BGEM, ICAOData (Point {pntLat = 68.7, pntLon = -52.75, pntEle = Just 43.0, pntTime = Nothing}))
  , (BGFH, ICAOData (Point {pntLat = 62.0, pntLon = -49.666666666666664, pntEle = Just 13.0, pntTime = Nothing}))
  , (BGGD, ICAOData (Point {pntLat = 61.233333333333334, pntLon = -48.1, pntEle = Just 35.0, pntTime = Nothing}))
  , (BGGH, ICAOData (Point {pntLat = 64.16666666666667, pntLon = -51.75, pntEle = Just 80.0, pntTime = Nothing}))
  , (BGHB, ICAOData (Point {pntLat = 66.91666666666667, pntLon = -53.666666666666664, pntEle = Just 12.0, pntTime = Nothing}))
  , (BGJH, ICAOData (Point {pntLat = 60.71666666666667, pntLon = -46.05, pntEle = Just 32.0, pntTime = Nothing}))
  , (BGJN, ICAOData (Point {pntLat = 69.23333333333333, pntLon = -51.06666666666667, pntEle = Just 29.0, pntTime = Nothing}))
  , (BGKK, ICAOData (Point {pntLat = 65.58333333333333, pntLon = -37.15, pntEle = Just 35.0, pntTime = Nothing}))
  , (BGKT, ICAOData (Point {pntLat = 70.41666666666667, pntLon = -21.966666666666665, pntEle = Just 41.0, pntTime = Nothing}))
  , (BGPC, ICAOData (Point {pntLat = 60.05, pntLon = -43.166666666666664, pntEle = Just 88.0, pntTime = Nothing}))
  , (BGSC, ICAOData (Point {pntLat = 70.48333333333333, pntLon = -21.95, pntEle = Just 65.0, pntTime = Nothing}))
  , (BGSF, ICAOData (Point {pntLat = 67.01666666666667, pntLon = -50.7, pntEle = Just 50.0, pntTime = Nothing}))
  , (BGTL, ICAOData (Point {pntLat = 76.53333333333333, pntLon = -68.75, pntEle = Just 77.0, pntTime = Nothing}))
  , (BIAR, ICAOData (Point {pntLat = 65.68333333333334, pntLon = -18.083333333333332, pntEle = Just 23.0, pntTime = Nothing}))
  , (BIEG, ICAOData (Point {pntLat = 65.28333333333333, pntLon = -14.4, pntEle = Just 23.0, pntTime = Nothing}))
  , (BIGR, ICAOData (Point {pntLat = 66.53333333333333, pntLon = -18.016666666666666, pntEle = Just 15.0, pntTime = Nothing}))
  , (BIHN, ICAOData (Point {pntLat = 64.3, pntLon = -15.216666666666667, pntEle = Just 17.0, pntTime = Nothing}))
  , (BIKF, ICAOData (Point {pntLat = 63.96666666666667, pntLon = -22.6, pntEle = Just 52.0, pntTime = Nothing}))
  , (BIRG, ICAOData (Point {pntLat = 66.45, pntLon = -15.95, pntEle = Just 8.0, pntTime = Nothing}))
  , (BIRK, ICAOData (Point {pntLat = 64.13333333333334, pntLon = -21.9, pntEle = Just 54.0, pntTime = Nothing}))
  , (BIST, ICAOData (Point {pntLat = 65.08333333333333, pntLon = -22.733333333333334, pntEle = Just 8.0, pntTime = Nothing}))
  , (BIVM, ICAOData (Point {pntLat = 63.4, pntLon = -20.283333333333335, pntEle = Just 118.0, pntTime = Nothing}))
  , (BIVO, ICAOData (Point {pntLat = 65.7, pntLon = -14.816666666666666, pntEle = Just 44.0, pntTime = Nothing}))
  , (CWAC, ICAOData (Point {pntLat = 48.666666666666664, pntLon = -124.83333333333333, pntEle = Just 41.0, pntTime = Nothing}))
  , (CWAF, ICAOData (Point {pntLat = 48.083333333333336, pntLon = -69.55, pntEle = Just 5.0, pntTime = Nothing}))
  , (CWAG, ICAOData (Point {pntLat = 49.916666666666664, pntLon = -55.666666666666664, pntEle = Just 192.0, pntTime = Nothing}))
  , (CWAH, ICAOData (Point {pntLat = 45.85, pntLon = -64.26666666666667, pntEle = Just 22.0, pntTime = Nothing}))
  , (CWAJ, ICAOData (Point {pntLat = 42.25, pntLon = -81.9, pntEle = Just 178.0, pntTime = Nothing}))
  , (CWAN, ICAOData (Point {pntLat = 48.916666666666664, pntLon = -125.55, pntEle = Just 27.0, pntTime = Nothing}))
  , (CWAQ, ICAOData (Point {pntLat = 49.05, pntLon = -105.48333333333333, pntEle = Just 756.0, pntTime = Nothing}))
  , (CWAR, ICAOData (Point {pntLat = 47.3, pntLon = -54.0, pntEle = Just 16.0, pntTime = Nothing}))
  , (CWAS, ICAOData (Point {pntLat = 49.483333333333334, pntLon = -123.3, pntEle = Nothing, pntTime = Nothing}))
  , (CWAV, ICAOData (Point {pntLat = 51.766666666666666, pntLon = -114.68333333333334, pntEle = Just 1114.0, pntTime = Nothing}))
  , (CWAX, ICAOData (Point {pntLat = 51.36666666666667, pntLon = -55.63333333333333, pntEle = Just 106.0, pntTime = Nothing}))
  , (CWBA, ICAOData (Point {pntLat = 51.18333333333333, pntLon = -115.56666666666666, pntEle = Just 1397.0, pntTime = Nothing}))
  , (CWBE, ICAOData (Point {pntLat = 45.96666666666667, pntLon = -81.48333333333333, pntEle = Just 196.0, pntTime = Nothing}))
  , (CWBG, ICAOData (Point {pntLat = 51.25, pntLon = -123.08333333333333, pntEle = Just 1670.0, pntTime = Nothing}))
  , (CWBI, ICAOData (Point {pntLat = 45.8, pntLon = -80.53333333333333, pntEle = Just 190.0, pntTime = Nothing}))
  , (CWBJ, ICAOData (Point {pntLat = 61.916666666666664, pntLon = -113.73333333333333, pntEle = Just 165.0, pntTime = Nothing}))
  , (CWBK, ICAOData (Point {pntLat = 45.766666666666666, pntLon = -62.68333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (CWBL, ICAOData (Point {pntLat = 51.75, pntLon = -99.9, pntEle = Just 256.0, pntTime = Nothing}))
  , (CWBM, ICAOData (Point {pntLat = 55.2, pntLon = -119.4, pntEle = Just 745.0, pntTime = Nothing}))
  , (CWBO, ICAOData (Point {pntLat = 50.55, pntLon = -111.85, pntEle = Just 747.0, pntTime = Nothing}))
  , (CWBR, ICAOData (Point {pntLat = 66.03333333333333, pntLon = -91.83333333333333, pntEle = Just 31.0, pntTime = Nothing}))
  , (CWBT, ICAOData (Point {pntLat = 50.266666666666666, pntLon = -64.23333333333333, pntEle = Just 11.0, pntTime = Nothing}))
  , (CWBV, ICAOData (Point {pntLat = 44.81666666666667, pntLon = -62.333333333333336, pntEle = Just 14.0, pntTime = Nothing}))
  , (CWBY, ICAOData (Point {pntLat = 49.833333333333336, pntLon = -64.3, pntEle = Just 53.0, pntTime = Nothing}))
  , (CWBZ, ICAOData (Point {pntLat = 45.11666666666667, pntLon = -74.28333333333333, pntEle = Just 49.0, pntTime = Nothing}))
  , (CWCA, ICAOData (Point {pntLat = 53.7, pntLon = -57.03333333333333, pntEle = Just 14.0, pntTime = Nothing}))
  , (CWCD, ICAOData (Point {pntLat = 52.15, pntLon = -106.55, pntEle = Just 510.0, pntTime = Nothing}))
  , (CWCF, ICAOData (Point {pntLat = 52.35, pntLon = -97.03333333333333, pntEle = Just 222.0, pntTime = Nothing}))
  , (CWCH, ICAOData (Point {pntLat = 48.75, pntLon = -91.61666666666666, pntEle = Just 424.0, pntTime = Nothing}))
  , (CWCI, ICAOData (Point {pntLat = 47.333333333333336, pntLon = -85.83333333333333, pntEle = Just 187.0, pntTime = Nothing}))
  , (CWCJ, ICAOData (Point {pntLat = 48.6, pntLon = -86.3, pntEle = Just 206.0, pntTime = Nothing}))
  , (CWCL, ICAOData (Point {pntLat = 51.15, pntLon = -121.5, pntEle = Just 1057.0, pntTime = Nothing}))
  , (CWCM, ICAOData (Point {pntLat = 49.9, pntLon = -99.35, pntEle = Just 384.0, pntTime = Nothing}))
  , (CWCO, ICAOData (Point {pntLat = 44.5, pntLon = -80.21666666666667, pntEle = Just 180.0, pntTime = Nothing}))
  , (CWCU, ICAOData (Point {pntLat = 44.38333333333333, pntLon = -79.78333333333333, pntEle = Just 295.0, pntTime = Nothing}))
  , (CWCX, ICAOData (Point {pntLat = 66.0, pntLon = -117.76666666666667, pntEle = Just 185.0, pntTime = Nothing}))
  , (CWDA, ICAOData (Point {pntLat = 50.71666666666667, pntLon = -56.11666666666667, pntEle = Just 29.0, pntTime = Nothing}))
  , (CWDB, ICAOData (Point {pntLat = 61.36666666666667, pntLon = -139.05, pntEle = Just 806.0, pntTime = Nothing}))
  , (CWDC, ICAOData (Point {pntLat = 59.56666666666667, pntLon = -108.48333333333333, pntEle = Just 318.0, pntTime = Nothing}))
  , (CWDE, ICAOData (Point {pntLat = 51.55, pntLon = -71.11666666666666, pntEle = Just 549.0, pntTime = Nothing}))
  , (CWDF, ICAOData (Point {pntLat = 44.733333333333334, pntLon = -81.28333333333333, pntEle = Just 182.0, pntTime = Nothing}))
  , (CWDH, ICAOData (Point {pntLat = 50.233333333333334, pntLon = -57.583333333333336, pntEle = Just 18.0, pntTime = Nothing}))
  , (CWDI, ICAOData (Point {pntLat = 48.96666666666667, pntLon = -56.06666666666667, pntEle = Just 105.0, pntTime = Nothing}))
  , (CWDJ, ICAOData (Point {pntLat = 50.4, pntLon = -104.58333333333333, pntEle = Just 573.0, pntTime = Nothing}))
  , (CWDK, ICAOData (Point {pntLat = 50.016666666666666, pntLon = -113.63333333333334, pntEle = Just 1012.0, pntTime = Nothing}))
  , (CWDL, ICAOData (Point {pntLat = 58.416666666666664, pntLon = -130.0, pntEle = Just 816.0, pntTime = Nothing}))
  , (CWDM, ICAOData (Point {pntLat = 50.46666666666667, pntLon = -59.63333333333333, pntEle = Just 6.0, pntTime = Nothing}))
  , (CWDO, ICAOData (Point {pntLat = 49.68333333333333, pntLon = -54.8, pntEle = Just 92.0, pntTime = Nothing}))
  , (CWDP, ICAOData (Point {pntLat = 50.666666666666664, pntLon = -70.53333333333333, pntEle = Just 496.0, pntTime = Nothing}))
  , (CWDQ, ICAOData (Point {pntLat = 47.416666666666664, pntLon = -72.8, pntEle = Just 169.0, pntTime = Nothing}))
  , (CWDS, ICAOData (Point {pntLat = 46.916666666666664, pntLon = -55.38333333333333, pntEle = Just 46.0, pntTime = Nothing}))
  , (CWDT, ICAOData (Point {pntLat = 49.9, pntLon = -71.25, pntEle = Just 399.0, pntTime = Nothing}))
  , (CWDU, ICAOData (Point {pntLat = 51.083333333333336, pntLon = -114.21666666666667, pntEle = Just 1235.0, pntTime = Nothing}))
  , (CWDV, ICAOData (Point {pntLat = 49.03333333333333, pntLon = -90.46666666666667, pntEle = Just 489.0, pntTime = Nothing}))
  , (CWEA, ICAOData (Point {pntLat = 50.18333333333333, pntLon = -96.06666666666666, pntEle = Just 268.0, pntTime = Nothing}))
  , (CWEB, ICAOData (Point {pntLat = 49.38333333333333, pntLon = -126.55, pntEle = Just 7.0, pntTime = Nothing}))
  , (CWEC, ICAOData (Point {pntLat = 48.36666666666667, pntLon = -89.11666666666666, pntEle = Just 209.0, pntTime = Nothing}))
  , (CWEE, ICAOData (Point {pntLat = 49.28333333333333, pntLon = -73.35, pntEle = Just 305.0, pntTime = Nothing}))
  , (CWEF, ICAOData (Point {pntLat = 47.233333333333334, pntLon = -60.13333333333333, pntEle = Just 26.0, pntTime = Nothing}))
  , (CWEH, ICAOData (Point {pntLat = 49.43333333333333, pntLon = -108.98333333333333, pntEle = Just 1078.0, pntTime = Nothing}))
  , (CWEI, ICAOData (Point {pntLat = 49.28333333333333, pntLon = -100.98333333333333, pntEle = Just 446.0, pntTime = Nothing}))
  , (CWEK, ICAOData (Point {pntLat = 54.583333333333336, pntLon = -130.7, pntEle = Just 8.0, pntTime = Nothing}))
  , (CWEL, ICAOData (Point {pntLat = 49.21666666666667, pntLon = -123.8, pntEle = Just 5.0, pntTime = Nothing}))
  , (CWEO, ICAOData (Point {pntLat = 51.86666666666667, pntLon = -63.28333333333333, pntEle = Just 561.0, pntTime = Nothing}))
  , (CWEP, ICAOData (Point {pntLat = 46.45, pntLon = -61.96666666666667, pntEle = Just 11.0, pntTime = Nothing}))
  , (CWEQ, ICAOData (Point {pntLat = 52.11666666666667, pntLon = -101.23333333333333, pntEle = Just 335.0, pntTime = Nothing}))
  , (CWER, ICAOData (Point {pntLat = 47.0, pntLon = -70.81666666666666, pntEle = Just 5.0, pntTime = Nothing}))
  , (CWES, ICAOData (Point {pntLat = 50.78333333333333, pntLon = -128.43333333333334, pntEle = Just 70.0, pntTime = Nothing}))
  , (CWEU, ICAOData (Point {pntLat = 79.98333333333333, pntLon = -85.93333333333334, pntEle = Just 10.0, pntTime = Nothing}))
  , (CWEV, ICAOData (Point {pntLat = 67.7, pntLon = -104.46666666666667, pntEle = Just 42.0, pntTime = Nothing}))
  , (CWEW, ICAOData (Point {pntLat = 45.81666666666667, pntLon = -73.43333333333334, pntEle = Just 21.0, pntTime = Nothing}))
  , (CWEZ, ICAOData (Point {pntLat = 48.78333333333333, pntLon = -123.05, pntEle = Just 24.0, pntTime = Nothing}))
  , (CWFD, ICAOData (Point {pntLat = 66.58333333333333, pntLon = -61.61666666666667, pntEle = Just 393.0, pntTime = Nothing}))
  , (CWFE, ICAOData (Point {pntLat = 53.68333333333333, pntLon = -112.86666666666666, pntEle = Just 716.0, pntTime = Nothing}))
  , (CWFF, ICAOData (Point {pntLat = 52.81666666666667, pntLon = -104.6, pntEle = Just 490.0, pntTime = Nothing}))
  , (CWFG, ICAOData (Point {pntLat = 50.81666666666667, pntLon = -128.9, pntEle = Just 112.0, pntTime = Nothing}))
  , (CWFJ, ICAOData (Point {pntLat = 49.2, pntLon = -113.28333333333333, pntEle = Just 1136.0, pntTime = Nothing}))
  , (CWFN, ICAOData (Point {pntLat = 57.35, pntLon = -107.13333333333334, pntEle = Just 495.0, pntTime = Nothing}))
  , (CWFQ, ICAOData (Point {pntLat = 45.05, pntLon = -72.83333333333333, pntEle = Just 152.0, pntTime = Nothing}))
  , (CWFW, ICAOData (Point {pntLat = 49.266666666666666, pntLon = -68.15, pntEle = Just 129.0, pntTime = Nothing}))
  , (CWFX, ICAOData (Point {pntLat = 67.03333333333333, pntLon = -126.08333333333333, pntEle = Just 259.0, pntTime = Nothing}))
  , (CWFZ, ICAOData (Point {pntLat = 62.71666666666667, pntLon = -109.18333333333334, pntEle = Just 168.0, pntTime = Nothing}))
  , (CWGB, ICAOData (Point {pntLat = 49.35, pntLon = -124.16666666666667, pntEle = Just 5.0, pntTime = Nothing}))
  , (CWGD, ICAOData (Point {pntLat = 43.766666666666666, pntLon = -81.71666666666667, pntEle = Just 214.0, pntTime = Nothing}))
  , (CWGF, ICAOData (Point {pntLat = 58.68333333333333, pntLon = -113.88333333333334, pntEle = Just 241.0, pntTime = Nothing}))
  , (CWGH, ICAOData (Point {pntLat = 44.416666666666664, pntLon = -75.85, pntEle = Just 82.0, pntTime = Nothing}))
  , (CWGL, ICAOData (Point {pntLat = 44.53333333333333, pntLon = -79.21666666666667, pntEle = Just 221.0, pntTime = Nothing}))
  , (CWGM, ICAOData (Point {pntLat = 59.13333333333333, pntLon = -113.8, pntEle = Just 1296.0, pntTime = Nothing}))
  , (CWGN, ICAOData (Point {pntLat = 49.03333333333333, pntLon = -97.56666666666666, pntEle = Just 251.0, pntTime = Nothing}))
  , (CWGP, ICAOData (Point {pntLat = 50.3, pntLon = -122.73333333333333, pntEle = Just 204.0, pntTime = Nothing}))
  , (CWGQ, ICAOData (Point {pntLat = 48.63333333333333, pntLon = -79.45, pntEle = Just 269.0, pntTime = Nothing}))
  , (CWGR, ICAOData (Point {pntLat = 47.416666666666664, pntLon = -61.8, pntEle = Just 10.0, pntTime = Nothing}))
  , (CWGT, ICAOData (Point {pntLat = 49.483333333333334, pntLon = -124.43333333333334, pntEle = Just 5.0, pntTime = Nothing}))
  , (CWGU, ICAOData (Point {pntLat = 51.666666666666664, pntLon = -98.75, pntEle = Just 265.0, pntTime = Nothing}))
  , (CWGW, ICAOData (Point {pntLat = 49.75, pntLon = -114.9, pntEle = Just 1137.0, pntTime = Nothing}))
  , (CWGY, ICAOData (Point {pntLat = 51.666666666666664, pntLon = -110.2, pntEle = Just 707.0, pntTime = Nothing}))
  , (CWHA, ICAOData (Point {pntLat = 53.45, pntLon = -114.46666666666667, pntEle = Just 747.0, pntTime = Nothing}))
  , (CWHC, ICAOData (Point {pntLat = 49.3, pntLon = -123.11666666666666, pntEle = Just 2.0, pntTime = Nothing}))
  , (CWHE, ICAOData (Point {pntLat = 53.416666666666664, pntLon = -113.2, pntEle = Just 694.0, pntTime = Nothing}))
  , (CWHH, ICAOData (Point {pntLat = 53.03333333333333, pntLon = -100.93333333333334, pntEle = Just 256.0, pntTime = Nothing}))
  , (CWHI, ICAOData (Point {pntLat = 51.7, pntLon = -113.21666666666667, pntEle = Just 907.0, pntTime = Nothing}))
  , (CWHL, ICAOData (Point {pntLat = 54.166666666666664, pntLon = -130.36666666666667, pntEle = Just 5.0, pntTime = Nothing}))
  , (CWHM, ICAOData (Point {pntLat = 45.71666666666667, pntLon = -73.38333333333334, pntEle = Just 19.0, pntTime = Nothing}))
  , (CWHO, ICAOData (Point {pntLat = 55.45, pntLon = -60.233333333333334, pntEle = Just 8.0, pntTime = Nothing}))
  , (CWHP, ICAOData (Point {pntLat = 49.083333333333336, pntLon = -61.7, pntEle = Just 4.0, pntTime = Nothing}))
  , (CWHQ, ICAOData (Point {pntLat = 46.68333333333333, pntLon = -71.95, pntEle = Just 55.0, pntTime = Nothing}))
  , (CWHS, ICAOData (Point {pntLat = 68.78333333333333, pntLon = -114.83333333333333, pntEle = Just 65.0, pntTime = Nothing}))
  , (CWHT, ICAOData (Point {pntLat = 60.766666666666666, pntLon = -137.58333333333334, pntEle = Just 599.0, pntTime = Nothing}))
  , (CWHV, ICAOData (Point {pntLat = 46.2, pntLon = -70.78333333333333, pntEle = Just 229.0, pntTime = Nothing}))
  , (CWHW, ICAOData (Point {pntLat = 49.266666666666666, pntLon = -54.88333333333333, pntEle = Just 96.0, pntTime = Nothing}))
  , (CWHY, ICAOData (Point {pntLat = 45.31666666666667, pntLon = -72.25, pntEle = Just 851.0, pntTime = Nothing}))
  , (CWIC, ICAOData (Point {pntLat = 78.78333333333333, pntLon = -103.55, pntEle = Just 58.0, pntTime = Nothing}))
  , (CWID, ICAOData (Point {pntLat = 61.31666666666667, pntLon = -117.6, pntEle = Just 161.0, pntTime = Nothing}))
  , (CWIE, ICAOData (Point {pntLat = 64.38333333333334, pntLon = -115.01666666666667, pntEle = Just 478.0, pntTime = Nothing}))
  , (CWII, ICAOData (Point {pntLat = 50.7, pntLon = -96.56666666666666, pntEle = Just 220.0, pntTime = Nothing}))
  , (CWIJ, ICAOData (Point {pntLat = 65.76666666666667, pntLon = -111.23333333333333, pntEle = Just 500.0, pntTime = Nothing}))
  , (CWIK, ICAOData (Point {pntLat = 50.38333333333333, pntLon = -102.68333333333334, pntEle = Just 598.0, pntTime = Nothing}))
  , (CWIL, ICAOData (Point {pntLat = 68.31666666666666, pntLon = -100.08333333333333, pntEle = Just 36.0, pntTime = Nothing}))
  , (CWIO, ICAOData (Point {pntLat = 56.733333333333334, pntLon = -131.66666666666666, pntEle = Just 15.0, pntTime = Nothing}))
  , (CWIP, ICAOData (Point {pntLat = 50.166666666666664, pntLon = -66.43333333333334, pntEle = Just 25.0, pntTime = Nothing}))
  , (CWIQ, ICAOData (Point {pntLat = 54.75, pntLon = -110.05, pntEle = Just 702.0, pntTime = Nothing}))
  , (CWIS, ICAOData (Point {pntLat = 47.28333333333333, pntLon = -70.63333333333334, pntEle = Just 719.0, pntTime = Nothing}))
  , (CWIT, ICAOData (Point {pntLat = 45.166666666666664, pntLon = -73.68333333333334, pntEle = Just 52.0, pntTime = Nothing}))
  , (CWIW, ICAOData (Point {pntLat = 51.666666666666664, pntLon = -105.4, pntEle = Just 526.0, pntTime = Nothing}))
  , (CWIX, ICAOData (Point {pntLat = 48.766666666666666, pntLon = -71.71666666666667, pntEle = Just 113.0, pntTime = Nothing}))
  , (CWIZ, ICAOData (Point {pntLat = 45.3, pntLon = -73.35, pntEle = Just 45.0, pntTime = Nothing}))
  , (CWJA, ICAOData (Point {pntLat = 52.88333333333333, pntLon = -118.06666666666666, pntEle = Just 1061.0, pntTime = Nothing}))
  , (CWJB, ICAOData (Point {pntLat = 47.3, pntLon = -71.26666666666667, pntEle = Just 91.0, pntTime = Nothing}))
  , (CWJC, ICAOData (Point {pntLat = 61.13333333333333, pntLon = -100.9, pntEle = Just 357.0, pntTime = Nothing}))
  , (CWJD, ICAOData (Point {pntLat = 53.18333333333333, pntLon = -99.26666666666667, pntEle = Just 223.0, pntTime = Nothing}))
  , (CWJH, ICAOData (Point {pntLat = 56.333333333333336, pntLon = -103.28333333333333, pntEle = Just 344.0, pntTime = Nothing}))
  , (CWJI, ICAOData (Point {pntLat = 49.733333333333334, pntLon = -105.93333333333334, pntEle = Just 724.0, pntTime = Nothing}))
  , (CWJM, ICAOData (Point {pntLat = 44.7, pntLon = -76.3, pntEle = Just 125.0, pntTime = Nothing}))
  , (CWJN, ICAOData (Point {pntLat = 69.56666666666666, pntLon = -138.91666666666666, pntEle = Just 1.0, pntTime = Nothing}))
  , (CWJO, ICAOData (Point {pntLat = 48.416666666666664, pntLon = -71.21666666666667, pntEle = Just 133.0, pntTime = Nothing}))
  , (CWJP, ICAOData (Point {pntLat = 61.666666666666664, pntLon = -108.41666666666667, pntEle = Just 396.0, pntTime = Nothing}))
  , (CWJR, ICAOData (Point {pntLat = 49.083333333333336, pntLon = -116.5, pntEle = Just 646.0, pntTime = Nothing}))
  , (CWJT, ICAOData (Point {pntLat = 46.06666666666667, pntLon = -74.53333333333333, pntEle = Just 239.0, pntTime = Nothing}))
  , (CWJW, ICAOData (Point {pntLat = 52.93333333333333, pntLon = -118.31666666666666, pntEle = Just 1020.0, pntTime = Nothing}))
  , (CWJX, ICAOData (Point {pntLat = 50.9, pntLon = -109.5, pntEle = Just 672.0, pntTime = Nothing}))
  , (CWJY, ICAOData (Point {pntLat = 62.7, pntLon = -98.3, pntEle = Just 148.0, pntTime = Nothing}))
  , (CWJZ, ICAOData (Point {pntLat = 63.233333333333334, pntLon = -101.76666666666667, pntEle = Just 237.0, pntTime = Nothing}))
  , (CWKD, ICAOData (Point {pntLat = 50.733333333333334, pntLon = -71.01666666666667, pntEle = Just 497.0, pntTime = Nothing}))
  , (CWKE, ICAOData (Point {pntLat = 68.43333333333334, pntLon = -89.71666666666667, pntEle = Just 326.0, pntTime = Nothing}))
  , (CWKG, ICAOData (Point {pntLat = 44.43333333333333, pntLon = -65.2, pntEle = Just 127.0, pntTime = Nothing}))
  , (CWKH, ICAOData (Point {pntLat = 48.583333333333336, pntLon = -123.58333333333333, pntEle = Just 366.0, pntTime = Nothing}))
  , (CWKK, ICAOData (Point {pntLat = 49.71666666666667, pntLon = -88.33333333333333, pntEle = Just 260.0, pntTime = Nothing}))
  , (CWKM, ICAOData (Point {pntLat = 69.61666666666666, pntLon = -140.2, pntEle = Just 13.0, pntTime = Nothing}))
  , (CWKO, ICAOData (Point {pntLat = 49.166666666666664, pntLon = -105.98333333333333, pntEle = Just 915.0, pntTime = Nothing}))
  , (CWKW, ICAOData (Point {pntLat = 59.983333333333334, pntLon = -64.16666666666667, pntEle = Just 551.0, pntTime = Nothing}))
  , (CWKX, ICAOData (Point {pntLat = 58.416666666666664, pntLon = -130.0, pntEle = Just 816.0, pntTime = Nothing}))
  , (CWLA, ICAOData (Point {pntLat = 54.25, pntLon = -133.13333333333333, pntEle = Just 41.0, pntTime = Nothing}))
  , (CWLB, ICAOData (Point {pntLat = 54.766666666666666, pntLon = -112.01666666666667, pntEle = Just 565.0, pntTime = Nothing}))
  , (CWLE, ICAOData (Point {pntLat = 50.95, pntLon = -107.15, pntEle = Just 665.0, pntTime = Nothing}))
  , (CWLF, ICAOData (Point {pntLat = 52.233333333333334, pntLon = -87.88333333333334, pntEle = Just 242.0, pntTime = Nothing}))
  , (CWLG, ICAOData (Point {pntLat = 67.2, pntLon = -130.21666666666667, pntEle = Just 63.0, pntTime = Nothing}))
  , (CWLI, ICAOData (Point {pntLat = 69.6, pntLon = -130.9, pntEle = Just 102.0, pntTime = Nothing}))
  , (CWLM, ICAOData (Point {pntLat = 48.416666666666664, pntLon = -123.31666666666666, pntEle = Just 70.0, pntTime = Nothing}))
  , (CWLO, ICAOData (Point {pntLat = 49.78333333333333, pntLon = -99.63333333333334, pntEle = Just 373.0, pntTime = Nothing}))
  , (CWLP, ICAOData (Point {pntLat = 50.93333333333333, pntLon = -127.63333333333334, pntEle = Just 17.0, pntTime = Nothing}))
  , (CWLQ, ICAOData (Point {pntLat = 46.31666666666667, pntLon = -79.46666666666667, pntEle = Just 204.0, pntTime = Nothing}))
  , (CWLS, ICAOData (Point {pntLat = 43.983333333333334, pntLon = -80.75, pntEle = Just 415.0, pntTime = Nothing}))
  , (CWLT, ICAOData (Point {pntLat = 82.5, pntLon = -62.333333333333336, pntEle = Just 63.0, pntTime = Nothing}))
  , (CWLV, ICAOData (Point {pntLat = 53.916666666666664, pntLon = -106.06666666666666, pntEle = Just 569.0, pntTime = Nothing}))
  , (CWLX, ICAOData (Point {pntLat = 68.88333333333334, pntLon = -75.13333333333334, pntEle = Just 162.0, pntTime = Nothing}))
  , (CWLY, ICAOData (Point {pntLat = 50.233333333333334, pntLon = -121.58333333333333, pntEle = Just 229.0, pntTime = Nothing}))
  , (CWLZ, ICAOData (Point {pntLat = 53.733333333333334, pntLon = -105.26666666666667, pntEle = Just 503.0, pntTime = Nothing}))
  , (CWME, ICAOData (Point {pntLat = 52.18333333333333, pntLon = -127.46666666666667, pntEle = Just 32.0, pntTime = Nothing}))
  , (CWMH, ICAOData (Point {pntLat = 52.3, pntLon = -55.833333333333336, pntEle = Just 10.0, pntTime = Nothing}))
  , (CWMI, ICAOData (Point {pntLat = 48.016666666666666, pntLon = -64.5, pntEle = Just 3.0, pntTime = Nothing}))
  , (CWMJ, ICAOData (Point {pntLat = 46.28333333333333, pntLon = -76.0, pntEle = Just 200.0, pntTime = Nothing}))
  , (CWMM, ICAOData (Point {pntLat = 49.2, pntLon = -122.68333333333334, pntEle = Just 5.0, pntTime = Nothing}))
  , (CWMN, ICAOData (Point {pntLat = 45.416666666666664, pntLon = -73.93333333333334, pntEle = Just 63.0, pntTime = Nothing}))
  , (CWMP, ICAOData (Point {pntLat = 61.05, pntLon = -109.33333333333333, pntEle = Just 396.0, pntTime = Nothing}))
  , (CWMQ, ICAOData (Point {pntLat = 49.9, pntLon = -109.46666666666667, pntEle = Just 767.0, pntTime = Nothing}))
  , (CWMS, ICAOData (Point {pntLat = 52.266666666666666, pntLon = -128.71666666666667, pntEle = Just 25.0, pntTime = Nothing}))
  , (CWMT, ICAOData (Point {pntLat = 63.15, pntLon = -117.26666666666667, pntEle = Just 271.0, pntTime = Nothing}))
  , (CWMU, ICAOData (Point {pntLat = 65.95, pntLon = -130.46666666666667, pntEle = Just 155.0, pntTime = Nothing}))
  , (CWMV, ICAOData (Point {pntLat = 64.75, pntLon = -124.21666666666667, pntEle = Just 341.0, pntTime = Nothing}))
  , (CWMW, ICAOData (Point {pntLat = 46.38333333333333, pntLon = -75.96666666666667, pntEle = Just 170.0, pntTime = Nothing}))
  , (CWMX, ICAOData (Point {pntLat = 57.53333333333333, pntLon = -111.56666666666666, pntEle = Just 310.0, pntTime = Nothing}))
  , (CWMZ, ICAOData (Point {pntLat = 45.03333333333333, pntLon = -80.36666666666666, pntEle = Just 191.0, pntTime = Nothing}))
  , (CWNB, ICAOData (Point {pntLat = 41.833333333333336, pntLon = -82.46666666666667, pntEle = Just 195.0, pntTime = Nothing}))
  , (CWNC, ICAOData (Point {pntLat = 43.95, pntLon = -78.16666666666667, pntEle = Just 78.0, pntTime = Nothing}))
  , (CWND, ICAOData (Point {pntLat = 69.63333333333334, pntLon = -135.43333333333334, pntEle = Just 12.0, pntTime = Nothing}))
  , (CWNH, ICAOData (Point {pntLat = 47.8, pntLon = -69.55, pntEle = Just 148.0, pntTime = Nothing}))
  , (CWNK, ICAOData (Point {pntLat = 49.5, pntLon = -98.03333333333333, pntEle = Just 268.0, pntTime = Nothing}))
  , (CWNL, ICAOData (Point {pntLat = 45.63333333333333, pntLon = -82.96666666666667, pntEle = Just 183.0, pntTime = Nothing}))
  , (CWNM, ICAOData (Point {pntLat = 49.5, pntLon = -117.3, pntEle = Just 535.0, pntTime = Nothing}))
  , (CWNO, ICAOData (Point {pntLat = 66.86666666666666, pntLon = -134.2, pntEle = Just 86.0, pntTime = Nothing}))
  , (CWNP, ICAOData (Point {pntLat = 50.266666666666666, pntLon = -117.81666666666666, pntEle = Just 524.0, pntTime = Nothing}))
  , (CWNQ, ICAOData (Point {pntLat = 46.21666666666667, pntLon = -72.65, pntEle = Just 10.0, pntTime = Nothing}))
  , (CWNR, ICAOData (Point {pntLat = 50.95, pntLon = -115.18333333333334, pntEle = Just 2543.0, pntTime = Nothing}))
  , (CWNT, ICAOData (Point {pntLat = 49.583333333333336, pntLon = -114.41666666666667, pntEle = Just 2164.0, pntTime = Nothing}))
  , (CWNW, ICAOData (Point {pntLat = 67.06666666666666, pntLon = -121.1, pntEle = Just 186.0, pntTime = Nothing}))
  , (CWNX, ICAOData (Point {pntLat = 53.68333333333333, pntLon = -124.83333333333333, pntEle = Just 715.0, pntTime = Nothing}))
  , (CWNZ, ICAOData (Point {pntLat = 49.75, pntLon = -84.16666666666667, pntEle = Just 259.0, pntTime = Nothing}))
  , (CWOA, ICAOData (Point {pntLat = 65.61666666666666, pntLon = -118.11666666666666, pntEle = Just 230.0, pntTime = Nothing}))
  , (CWOB, ICAOData (Point {pntLat = 63.333333333333336, pntLon = -64.15, pntEle = Just 367.0, pntTime = Nothing}))
  , (CWOC, ICAOData (Point {pntLat = 48.016666666666666, pntLon = -65.33333333333333, pntEle = Just 47.0, pntTime = Nothing}))
  , (CWOD, ICAOData (Point {pntLat = 48.833333333333336, pntLon = -72.55, pntEle = Just 137.0, pntTime = Nothing}))
  , (CWOE, ICAOData (Point {pntLat = 49.11666666666667, pntLon = -110.46666666666667, pntEle = Just 935.0, pntTime = Nothing}))
  , (CWOH, ICAOData (Point {pntLat = 46.05, pntLon = -74.28333333333333, pntEle = Just 395.0, pntTime = Nothing}))
  , (CWOI, ICAOData (Point {pntLat = 69.15, pntLon = -140.15, pntEle = Just 244.0, pntTime = Nothing}))
  , (CWOK, ICAOData (Point {pntLat = 51.13333333333333, pntLon = -106.58333333333333, pntEle = Just 595.0, pntTime = Nothing}))
  , (CWOU, ICAOData (Point {pntLat = 54.78333333333333, pntLon = -110.06666666666666, pntEle = Just 606.0, pntTime = Nothing}))
  , (CWPA, ICAOData (Point {pntLat = 53.78333333333333, pntLon = -118.43333333333334, pntEle = Just 1542.0, pntTime = Nothing}))
  , (CWPB, ICAOData (Point {pntLat = 44.85, pntLon = -79.86666666666666, pntEle = Just 183.0, pntTime = Nothing}))
  , (CWPC, ICAOData (Point {pntLat = 42.86666666666667, pntLon = -79.25, pntEle = Just 184.0, pntTime = Nothing}))
  , (CWPD, ICAOData (Point {pntLat = 47.56666666666667, pntLon = -71.23333333333333, pntEle = Just 803.0, pntTime = Nothing}))
  , (CWPF, ICAOData (Point {pntLat = 48.43333333333333, pntLon = -123.43333333333334, pntEle = Just 3.0, pntTime = Nothing}))
  , (CWPH, ICAOData (Point {pntLat = 58.45, pntLon = -78.11666666666666, pntEle = Just 3.0, pntTime = Nothing}))
  , (CWPI, ICAOData (Point {pntLat = 50.983333333333334, pntLon = -127.73333333333333, pntEle = Just 9.0, pntTime = Nothing}))
  , (CWPJ, ICAOData (Point {pntLat = 47.06666666666667, pntLon = -64.8, pntEle = Just 5.0, pntTime = Nothing}))
  , (CWPK, ICAOData (Point {pntLat = 47.916666666666664, pntLon = -74.61666666666666, pntEle = Just 442.0, pntTime = Nothing}))
  , (CWPL, ICAOData (Point {pntLat = 51.45, pntLon = -90.2, pntEle = Just 389.0, pntTime = Nothing}))
  , (CWPO, ICAOData (Point {pntLat = 49.2, pntLon = -98.9, pntEle = Just 470.0, pntTime = Nothing}))
  , (CWPQ, ICAOData (Point {pntLat = 45.63333333333333, pntLon = -70.55, pntEle = Just 51.0, pntTime = Nothing}))
  , (CWPR, ICAOData (Point {pntLat = 50.6, pntLon = -120.51666666666667, pntEle = Just 700.0, pntTime = Nothing}))
  , (CWPS, ICAOData (Point {pntLat = 42.56666666666667, pntLon = -80.05, pntEle = Just 175.0, pntTime = Nothing}))
  , (CWPU, ICAOData (Point {pntLat = 52.11666666666667, pntLon = -124.13333333333334, pntEle = Just 910.0, pntTime = Nothing}))
  , (CWQC, ICAOData (Point {pntLat = 49.25, pntLon = -124.83333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (CWQE, ICAOData (Point {pntLat = 43.61666666666667, pntLon = -79.35, pntEle = Just 87.0, pntTime = Nothing}))
  , (CWQF, ICAOData (Point {pntLat = 61.96666666666667, pntLon = -127.21666666666667, pntEle = Just 634.0, pntTime = Nothing}))
  , (CWQG, ICAOData (Point {pntLat = 46.9, pntLon = -71.5, pntEle = Just 168.0, pntTime = Nothing}))
  , (CWQH, ICAOData (Point {pntLat = 45.36666666666667, pntLon = -71.81666666666666, pntEle = Just 181.0, pntTime = Nothing}))
  , (CWQK, ICAOData (Point {pntLat = 48.3, pntLon = -123.53333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (CWQL, ICAOData (Point {pntLat = 49.7, pntLon = -112.78333333333333, pntEle = Just 921.0, pntTime = Nothing}))
  , (CWQM, ICAOData (Point {pntLat = 48.38333333333333, pntLon = -70.53333333333333, pntEle = Just 7.0, pntTime = Nothing}))
  , (CWQO, ICAOData (Point {pntLat = 48.4, pntLon = -68.88333333333334, pntEle = Just 7.0, pntTime = Nothing}))
  , (CWQP, ICAOData (Point {pntLat = 43.833333333333336, pntLon = -77.15, pntEle = Just 79.0, pntTime = Nothing}))
  , (CWQQ, ICAOData (Point {pntLat = 52.45, pntLon = -113.75, pntEle = Just 874.0, pntTime = Nothing}))
  , (CWQR, ICAOData (Point {pntLat = 50.21666666666667, pntLon = -64.2, pntEle = Just 9.0, pntTime = Nothing}))
  , (CWQS, ICAOData (Point {pntLat = 53.31666666666667, pntLon = -132.75, pntEle = Just 14.0, pntTime = Nothing}))
  , (CWQV, ICAOData (Point {pntLat = 48.266666666666666, pntLon = -70.11666666666666, pntEle = Just 0.0, pntTime = Nothing}))
  , (CWQW, ICAOData (Point {pntLat = 50.166666666666664, pntLon = -60.06666666666667, pntEle = Just 7.0, pntTime = Nothing}))
  , (CWQY, ICAOData (Point {pntLat = 63.583333333333336, pntLon = -105.15, pntEle = Just 317.0, pntTime = Nothing}))
  , (CWRA, ICAOData (Point {pntLat = 46.65, pntLon = -53.06666666666667, pntEle = Just 27.0, pntTime = Nothing}))
  , (CWRD, ICAOData (Point {pntLat = 56.53333333333333, pntLon = -115.26666666666667, pntEle = Just 546.0, pntTime = Nothing}))
  , (CWRF, ICAOData (Point {pntLat = 69.43333333333334, pntLon = -89.73333333333333, pntEle = Just 325.0, pntTime = Nothing}))
  , (CWRH, ICAOData (Point {pntLat = 61.583333333333336, pntLon = -64.65, pntEle = Just 369.0, pntTime = Nothing}))
  , (CWRJ, ICAOData (Point {pntLat = 51.56666666666667, pntLon = -107.91666666666667, pntEle = Just 586.0, pntTime = Nothing}))
  , (CWRN, ICAOData (Point {pntLat = 45.35, pntLon = -60.983333333333334, pntEle = Just 4.0, pntTime = Nothing}))
  , (CWRO, ICAOData (Point {pntLat = 54.166666666666664, pntLon = -131.66666666666666, pntEle = Just 7.0, pntTime = Nothing}))
  , (CWRP, ICAOData (Point {pntLat = 69.93333333333334, pntLon = -128.96666666666667, pntEle = Just 89.0, pntTime = Nothing}))
  , (CWRT, ICAOData (Point {pntLat = 49.63333333333333, pntLon = -114.48333333333333, pntEle = Just 1303.0, pntTime = Nothing}))
  , (CWRU, ICAOData (Point {pntLat = 50.11666666666667, pntLon = -127.93333333333334, pntEle = Just 99.0, pntTime = Nothing}))
  , (CWRV, ICAOData (Point {pntLat = 53.03333333333333, pntLon = -112.81666666666666, pntEle = Just 739.0, pntTime = Nothing}))
  , (CWRW, ICAOData (Point {pntLat = 45.71666666666667, pntLon = -60.233333333333334, pntEle = Just 10.0, pntTime = Nothing}))
  , (CWRY, ICAOData (Point {pntLat = 49.13333333333333, pntLon = -112.05, pntEle = Just 1050.0, pntTime = Nothing}))
  , (CWRZ, ICAOData (Point {pntLat = 48.416666666666664, pntLon = -64.31666666666666, pntEle = Just 16.0, pntTime = Nothing}))
  , (CWSA, ICAOData (Point {pntLat = 43.93333333333333, pntLon = -60.016666666666666, pntEle = Just 4.0, pntTime = Nothing}))
  , (CWSE, ICAOData (Point {pntLat = 53.53333333333333, pntLon = -114.1, pntEle = Just 766.0, pntTime = Nothing}))
  , (CWSF, ICAOData (Point {pntLat = 49.25, pntLon = -65.33333333333333, pntEle = Just 29.0, pntTime = Nothing}))
  , (CWSG, ICAOData (Point {pntLat = 49.11666666666667, pntLon = -66.65, pntEle = Just 5.0, pntTime = Nothing}))
  , (CWSH, ICAOData (Point {pntLat = 58.583333333333336, pntLon = -118.5, pntEle = Just 373.0, pntTime = Nothing}))
  , (CWSI, ICAOData (Point {pntLat = 42.85, pntLon = -80.26666666666667, pntEle = Just 241.0, pntTime = Nothing}))
  , (CWSK, ICAOData (Point {pntLat = 49.78333333333333, pntLon = -123.16666666666667, pntEle = Just 59.0, pntTime = Nothing}))
  , (CWSL, ICAOData (Point {pntLat = 50.7, pntLon = -119.28333333333333, pntEle = Just 351.0, pntTime = Nothing}))
  , (CWSP, ICAOData (Point {pntLat = 48.38333333333333, pntLon = -123.91666666666667, pntEle = Just 21.0, pntTime = Nothing}))
  , (CWSQ, ICAOData (Point {pntLat = 74.13333333333334, pntLon = -119.98333333333333, pntEle = Just 32.0, pntTime = Nothing}))
  , (CWSR, ICAOData (Point {pntLat = 53.36666666666667, pntLon = -107.55, pntEle = Just 584.0, pntTime = Nothing}))
  , (CWSS, ICAOData (Point {pntLat = 45.21666666666667, pntLon = -67.25, pntEle = Just 26.0, pntTime = Nothing}))
  , (CWST, ICAOData (Point {pntLat = 47.35, pntLon = -70.03333333333333, pntEle = Just 31.0, pntTime = Nothing}))
  , (CWSY, ICAOData (Point {pntLat = 72.0, pntLon = -125.26666666666667, pntEle = Just 88.0, pntTime = Nothing}))
  , (CWSZ, ICAOData (Point {pntLat = 51.083333333333336, pntLon = -97.55, pntEle = Just 253.0, pntTime = Nothing}))
  , (CWTA, ICAOData (Point {pntLat = 45.5, pntLon = -73.58333333333333, pntEle = Just 63.0, pntTime = Nothing}))
  , (CWTB, ICAOData (Point {pntLat = 55.333333333333336, pntLon = -63.21666666666667, pntEle = Just 479.0, pntTime = Nothing}))
  , (CWTC, ICAOData (Point {pntLat = 53.05, pntLon = -129.68333333333334, pntEle = Just 10.0, pntTime = Nothing}))
  , (CWTD, ICAOData (Point {pntLat = 65.1, pntLon = -102.43333333333334, pntEle = Just 244.0, pntTime = Nothing}))
  , (CWTE, ICAOData (Point {pntLat = 60.43333333333333, pntLon = -121.23333333333333, pntEle = Just 498.0, pntTime = Nothing}))
  , (CWTF, ICAOData (Point {pntLat = 59.21666666666667, pntLon = -109.7, pntEle = Just 238.0, pntTime = Nothing}))
  , (CWTG, ICAOData (Point {pntLat = 49.31666666666667, pntLon = -67.38333333333334, pntEle = Just 5.0, pntTime = Nothing}))
  , (CWTN, ICAOData (Point {pntLat = 47.06666666666667, pntLon = -70.8, pntEle = Just 6.0, pntTime = Nothing}))
  , (CWTO, ICAOData (Point {pntLat = 43.78333333333333, pntLon = -79.46666666666667, pntEle = Just 187.0, pntTime = Nothing}))
  , (CWTY, ICAOData (Point {pntLat = 46.35, pntLon = -72.51666666666667, pntEle = Just 6.0, pntTime = Nothing}))
  , (CWUM, ICAOData (Point {pntLat = 62.233333333333334, pntLon = -133.35, pntEle = Just 717.0, pntTime = Nothing}))
  , (CWUP, ICAOData (Point {pntLat = 68.46666666666667, pntLon = -66.8, pntEle = Just 401.0, pntTime = Nothing}))
  , (CWUR, ICAOData (Point {pntLat = 45.36666666666667, pntLon = -63.266666666666666, pntEle = Just 40.0, pntTime = Nothing}))
  , (CWUS, ICAOData (Point {pntLat = 49.56666666666667, pntLon = -119.65, pntEle = Just 454.0, pntTime = Nothing}))
  , (CWUT, ICAOData (Point {pntLat = 50.45, pntLon = -100.6, pntEle = Just 561.0, pntTime = Nothing}))
  , (CWUU, ICAOData (Point {pntLat = 68.3, pntLon = -85.66666666666667, pntEle = Just 395.0, pntTime = Nothing}))
  , (CWUW, ICAOData (Point {pntLat = 68.65, pntLon = -71.16666666666667, pntEle = Just 518.0, pntTime = Nothing}))
  , (CWUX, ICAOData (Point {pntLat = 48.3, pntLon = -70.93333333333334, pntEle = Just 135.0, pntTime = Nothing}))
  , (CWUY, ICAOData (Point {pntLat = 48.25, pntLon = -79.03333333333333, pntEle = Just 318.0, pntTime = Nothing}))
  , (CWVA, ICAOData (Point {pntLat = 48.666666666666664, pntLon = -53.11666666666667, pntEle = Just 25.0, pntTime = Nothing}))
  , (CWVC, ICAOData (Point {pntLat = 50.266666666666666, pntLon = -107.73333333333333, pntEle = Just 825.0, pntTime = Nothing}))
  , (CWVD, ICAOData (Point {pntLat = 67.53333333333333, pntLon = -63.78333333333333, pntEle = Just 584.0, pntTime = Nothing}))
  , (CWVH, ICAOData (Point {pntLat = 68.9, pntLon = -133.93333333333334, pntEle = Just 261.0, pntTime = Nothing}))
  , (CWVI, ICAOData (Point {pntLat = 53.61666666666667, pntLon = -112.03333333333333, pntEle = Just 639.0, pntTime = Nothing}))
  , (CWVK, ICAOData (Point {pntLat = 50.233333333333334, pntLon = -119.28333333333333, pntEle = Just 555.0, pntTime = Nothing}))
  , (CWVN, ICAOData (Point {pntLat = 49.06666666666667, pntLon = -107.58333333333333, pntEle = Just 785.0, pntTime = Nothing}))
  , (CWVO, ICAOData (Point {pntLat = 50.05, pntLon = -112.13333333333334, pntEle = Just 779.0, pntTime = Nothing}))
  , (CWVP, ICAOData (Point {pntLat = 49.65, pntLon = -109.51666666666667, pntEle = Just 1271.0, pntTime = Nothing}))
  , (CWVQ, ICAOData (Point {pntLat = 45.43333333333333, pntLon = -73.93333333333334, pntEle = Just 39.0, pntTime = Nothing}))
  , (CWVT, ICAOData (Point {pntLat = 55.833333333333336, pntLon = -108.43333333333334, pntEle = Just 434.0, pntTime = Nothing}))
  , (CWVU, ICAOData (Point {pntLat = 44.28333333333333, pntLon = -66.33333333333333, pntEle = Just 16.0, pntTime = Nothing}))
  , (CWVY, ICAOData (Point {pntLat = 46.43333333333333, pntLon = -71.93333333333334, pntEle = Just 108.0, pntTime = Nothing}))
  , (CWWA, ICAOData (Point {pntLat = 49.35, pntLon = -123.18333333333334, pntEle = Just 178.0, pntTime = Nothing}))
  , (CWWB, ICAOData (Point {pntLat = 43.3, pntLon = -79.8, pntEle = Just 77.0, pntTime = Nothing}))
  , (CWWC, ICAOData (Point {pntLat = 58.18333333333333, pntLon = -103.7, pntEle = Just 492.0, pntTime = Nothing}))
  , (CWWE, ICAOData (Point {pntLat = 43.983333333333334, pntLon = -64.66666666666667, pntEle = Just 9.0, pntTime = Nothing}))
  , (CWWF, ICAOData (Point {pntLat = 49.7, pntLon = -103.8, pntEle = Just 589.0, pntTime = Nothing}))
  , (CWWK, ICAOData (Point {pntLat = 49.016666666666666, pntLon = -122.78333333333333, pntEle = Just 15.0, pntTime = Nothing}))
  , (CWWL, ICAOData (Point {pntLat = 53.5, pntLon = -130.63333333333333, pntEle = Just 17.0, pntTime = Nothing}))
  , (CWWN, ICAOData (Point {pntLat = 54.983333333333334, pntLon = -85.43333333333334, pntEle = Just 52.0, pntTime = Nothing}))
  , (CWWO, ICAOData (Point {pntLat = 53.38333333333333, pntLon = -118.33333333333333, pntEle = Just 1402.0, pntTime = Nothing}))
  , (CWWP, ICAOData (Point {pntLat = 50.65, pntLon = -99.93333333333334, pntEle = Just 626.0, pntTime = Nothing}))
  , (CWWS, ICAOData (Point {pntLat = 52.8, pntLon = -97.61666666666666, pntEle = Just 223.0, pntTime = Nothing}))
  , (CWWV, ICAOData (Point {pntLat = 60.416666666666664, pntLon = -64.85, pntEle = Just 31.0, pntTime = Nothing}))
  , (CWWX, ICAOData (Point {pntLat = 45.333333333333336, pntLon = -81.73333333333333, pntEle = Just 181.0, pntTime = Nothing}))
  , (CWWZ, ICAOData (Point {pntLat = 43.25, pntLon = -79.21666666666667, pntEle = Just 79.0, pntTime = Nothing}))
  , (CWXA, ICAOData (Point {pntLat = 51.083333333333336, pntLon = -115.06666666666666, pntEle = Just 1298.0, pntTime = Nothing}))
  , (CWXC, ICAOData (Point {pntLat = 48.88333333333333, pntLon = -71.03333333333333, pntEle = Just 304.0, pntTime = Nothing}))
  , (CWXH, ICAOData (Point {pntLat = 50.63333333333333, pntLon = -128.11666666666667, pntEle = Just 568.0, pntTime = Nothing}))
  , (CWXL, ICAOData (Point {pntLat = 49.63333333333333, pntLon = -111.45, pntEle = Just 838.0, pntTime = Nothing}))
  , (CWXM, ICAOData (Point {pntLat = 50.4, pntLon = -125.86666666666666, pntEle = Just 19.0, pntTime = Nothing}))
  , (CWXP, ICAOData (Point {pntLat = 66.15, pntLon = -65.73333333333333, pntEle = Just 23.0, pntTime = Nothing}))
  , (CWXV, ICAOData (Point {pntLat = 66.08333333333333, pntLon = -96.5, pntEle = Just 28.0, pntTime = Nothing}))
  , (CWXW, ICAOData (Point {pntLat = 52.65, pntLon = -56.86666666666667, pntEle = Just 43.0, pntTime = Nothing}))
  , (CWYE, ICAOData (Point {pntLat = 51.483333333333334, pntLon = -107.05, pntEle = Just 541.0, pntTime = Nothing}))
  , (CWYH, ICAOData (Point {pntLat = 64.11666666666666, pntLon = -117.36666666666666, pntEle = Just 231.0, pntTime = Nothing}))
  , (CWYJ, ICAOData (Point {pntLat = 48.45, pntLon = -123.3, pntEle = Just 60.0, pntTime = Nothing}))
  , (CWYK, ICAOData (Point {pntLat = 57.13333333333333, pntLon = -61.483333333333334, pntEle = Just 834.0, pntTime = Nothing}))
  , (CWYL, ICAOData (Point {pntLat = 51.45, pntLon = -116.33333333333333, pntEle = Just 1615.0, pntTime = Nothing}))
  , (CWYM, ICAOData (Point {pntLat = 64.95, pntLon = -63.583333333333336, pntEle = Just 583.0, pntTime = Nothing}))
  , (CWYO, ICAOData (Point {pntLat = 51.766666666666666, pntLon = -104.2, pntEle = Just 561.0, pntTime = Nothing}))
  , (CWYY, ICAOData (Point {pntLat = 49.03333333333333, pntLon = -119.43333333333334, pntEle = Just 283.0, pntTime = Nothing}))
  , (CWZA, ICAOData (Point {pntLat = 49.25, pntLon = -121.76666666666667, pntEle = Just 15.0, pntTime = Nothing}))
  , (CWZB, ICAOData (Point {pntLat = 47.56666666666667, pntLon = -59.166666666666664, pntEle = Just 40.0, pntTime = Nothing}))
  , (CWZN, ICAOData (Point {pntLat = 47.36666666666667, pntLon = -55.8, pntEle = Just 35.0, pntTime = Nothing}))
  , (CWZQ, ICAOData (Point {pntLat = 46.55, pntLon = -61.05, pntEle = Just 13.0, pntTime = Nothing}))
  , (CWZS, ICAOData (Point {pntLat = 48.46666666666667, pntLon = -67.43333333333334, pntEle = Just 166.0, pntTime = Nothing}))
  , (CWZT, ICAOData (Point {pntLat = 51.1, pntLon = -100.05, pntEle = Just 305.0, pntTime = Nothing}))
  , (CWZV, ICAOData (Point {pntLat = 61.63333333333333, pntLon = -125.8, pntEle = Just 610.0, pntTime = Nothing}))
  , (CWZZ, ICAOData (Point {pntLat = 58.333333333333336, pntLon = -62.583333333333336, pntEle = Just 483.0, pntTime = Nothing}))
  , (CXBK, ICAOData (Point {pntLat = 50.2, pntLon = -104.7, pntEle = Just 580.0, pntTime = Nothing}))
  , (CXBO, ICAOData (Point {pntLat = 46.833333333333336, pntLon = -71.2, pntEle = Just 10.0, pntTime = Nothing}))
  , (CXCK, ICAOData (Point {pntLat = 62.11666666666667, pntLon = -136.18333333333334, pntEle = Just 632.0, pntTime = Nothing}))
  , (CXDE, ICAOData (Point {pntLat = 65.21666666666667, pntLon = -123.43333333333334, pntEle = Just 213.0, pntTime = Nothing}))
  , (CXDI, ICAOData (Point {pntLat = 42.86666666666667, pntLon = -80.55, pntEle = Just 232.0, pntTime = Nothing}))
  , (CXDW, ICAOData (Point {pntLat = 49.4, pntLon = -98.31666666666666, pntEle = Just 341.0, pntTime = Nothing}))
  , (CXEC, ICAOData (Point {pntLat = 53.56666666666667, pntLon = -113.51666666666667, pntEle = Just 671.0, pntTime = Nothing}))
  , (CXEG, ICAOData (Point {pntLat = 53.3, pntLon = -113.6, pntEle = Just 715.0, pntTime = Nothing}))
  , (CXGH, ICAOData (Point {pntLat = 50.61666666666667, pntLon = -96.96666666666667, pntEle = Just 217.0, pntTime = Nothing}))
  , (CXHF, ICAOData (Point {pntLat = 45.833333333333336, pntLon = -75.65, pntEle = Just 190.0, pntTime = Nothing}))
  , (CXKT, ICAOData (Point {pntLat = 45.06666666666667, pntLon = -64.48333333333333, pntEle = Just 49.0, pntTime = Nothing}))
  , (CXLC, ICAOData (Point {pntLat = 63.6, pntLon = -113.86666666666666, pntEle = Just 373.0, pntTime = Nothing}))
  , (CXLL, ICAOData (Point {pntLat = 61.11666666666667, pntLon = -122.85, pntEle = Just 183.0, pntTime = Nothing}))
  , (CXMD, ICAOData (Point {pntLat = 49.18333333333333, pntLon = -98.08333333333333, pntEle = Just 298.0, pntTime = Nothing}))
  , (CXMM, ICAOData (Point {pntLat = 56.65, pntLon = -111.21666666666667, pntEle = Just 369.0, pntTime = Nothing}))
  , (CXNM, ICAOData (Point {pntLat = 46.81666666666667, pntLon = -60.666666666666664, pntEle = Just 439.0, pntTime = Nothing}))
  , (CXOX, ICAOData (Point {pntLat = 55.15, pntLon = -105.26666666666667, pntEle = Just 378.0, pntTime = Nothing}))
  , (CXPV, ICAOData (Point {pntLat = 58.61666666666667, pntLon = -111.66666666666667, pntEle = Just 212.0, pntTime = Nothing}))
  , (CXQA, ICAOData (Point {pntLat = 68.25, pntLon = -122.1, pntEle = Just 530.0, pntTime = Nothing}))
  , (CXRH, ICAOData (Point {pntLat = 49.56666666666667, pntLon = -57.88333333333333, pntEle = Just 68.0, pntTime = Nothing}))
  , (CXSH, ICAOData (Point {pntLat = 46.583333333333336, pntLon = -72.58333333333333, pntEle = Just 110.0, pntTime = Nothing}))
  , (CXSR, ICAOData (Point {pntLat = 55.68333333333333, pntLon = -119.23333333333333, pntEle = Just 1015.0, pntTime = Nothing}))
  , (CXSW, ICAOData (Point {pntLat = 47.5, pntLon = -52.78333333333333, pntEle = Just 114.0, pntTime = Nothing}))
  , (CXTN, ICAOData (Point {pntLat = 69.18333333333334, pntLon = -122.35, pntEle = Just 552.0, pntTime = Nothing}))
  , (CXTP, ICAOData (Point {pntLat = 48.55, pntLon = -53.96666666666667, pntEle = Just 107.0, pntTime = Nothing}))
  , (CXTV, ICAOData (Point {pntLat = 68.75, pntLon = -133.5, pntEle = Just 85.0, pntTime = Nothing}))
  , (CXWN, ICAOData (Point {pntLat = 49.88333333333333, pntLon = -97.13333333333334, pntEle = Just 230.0, pntTime = Nothing}))
  , (CXXX, ICAOData (Point {pntLat = 58.38333333333333, pntLon = -109.5, pntEle = Just 339.0, pntTime = Nothing}))
  , (CXZU, ICAOData (Point {pntLat = 54.15, pntLon = -115.78333333333333, pntEle = Just 785.0, pntTime = Nothing}))
  , (CYAH, ICAOData (Point {pntLat = 53.75, pntLon = -73.66666666666667, pntEle = Just 306.0, pntTime = Nothing}))
  , (CYAJ, ICAOData (Point {pntLat = 69.58333333333333, pntLon = -140.18333333333334, pntEle = Just 7.0, pntTime = Nothing}))
  , (CYAM, ICAOData (Point {pntLat = 46.483333333333334, pntLon = -84.5, pntEle = Just 187.0, pntTime = Nothing}))
  , (CYAW, ICAOData (Point {pntLat = 44.63333333333333, pntLon = -63.5, pntEle = Just 51.0, pntTime = Nothing}))
  , (CYAZ, ICAOData (Point {pntLat = 49.083333333333336, pntLon = -125.76666666666667, pntEle = Just 24.0, pntTime = Nothing}))
  , (CYBC, ICAOData (Point {pntLat = 49.13333333333333, pntLon = -68.2, pntEle = Just 21.0, pntTime = Nothing}))
  , (CYBD, ICAOData (Point {pntLat = 52.38333333333333, pntLon = -126.58333333333333, pntEle = Just 35.0, pntTime = Nothing}))
  , (CYBG, ICAOData (Point {pntLat = 48.333333333333336, pntLon = -71.0, pntEle = Just 159.0, pntTime = Nothing}))
  , (CYBK, ICAOData (Point {pntLat = 64.3, pntLon = -96.08333333333333, pntEle = Just 18.0, pntTime = Nothing}))
  , (CYBR, ICAOData (Point {pntLat = 49.916666666666664, pntLon = -99.95, pntEle = Just 409.0, pntTime = Nothing}))
  , (CYBU, ICAOData (Point {pntLat = 53.333333333333336, pntLon = -104.0, pntEle = Just 372.0, pntTime = Nothing}))
  , (CYBV, ICAOData (Point {pntLat = 52.35, pntLon = -97.03333333333333, pntEle = Just 222.0, pntTime = Nothing}))
  , (CYBX, ICAOData (Point {pntLat = 51.45, pntLon = -57.18333333333333, pntEle = Just 30.0, pntTime = Nothing}))
  , (CYCB, ICAOData (Point {pntLat = 69.1, pntLon = -105.11666666666666, pntEle = Just 23.0, pntTime = Nothing}))
  , (CYCD, ICAOData (Point {pntLat = 49.05, pntLon = -123.86666666666666, pntEle = Just 28.0, pntTime = Nothing}))
  , (CYCG, ICAOData (Point {pntLat = 49.3, pntLon = -117.63333333333334, pntEle = Just 495.0, pntTime = Nothing}))
  , (CYCH, ICAOData (Point {pntLat = 47.0, pntLon = -65.45, pntEle = Just 31.0, pntTime = Nothing}))
  , (CYCL, ICAOData (Point {pntLat = 47.983333333333334, pntLon = -66.33333333333333, pntEle = Just 38.0, pntTime = Nothing}))
  , (CYCO, ICAOData (Point {pntLat = 67.81666666666666, pntLon = -115.13333333333334, pntEle = Just 22.0, pntTime = Nothing}))
  , (CYCP, ICAOData (Point {pntLat = 52.13333333333333, pntLon = -119.3, pntEle = Just 679.0, pntTime = Nothing}))
  , (CYCT, ICAOData (Point {pntLat = 52.06666666666667, pntLon = -111.45, pntEle = Just 791.0, pntTime = Nothing}))
  , (CYCX, ICAOData (Point {pntLat = 45.833333333333336, pntLon = -66.43333333333334, pntEle = Just 51.0, pntTime = Nothing}))
  , (CYCY, ICAOData (Point {pntLat = 70.48333333333333, pntLon = -68.51666666666667, pntEle = Just 25.0, pntTime = Nothing}))
  , (CYDA, ICAOData (Point {pntLat = 64.05, pntLon = -139.13333333333333, pntEle = Just 370.0, pntTime = Nothing}))
  , (CYDC, ICAOData (Point {pntLat = 49.46666666666667, pntLon = -120.51666666666667, pntEle = Just 700.0, pntTime = Nothing}))
  , (CYDF, ICAOData (Point {pntLat = 49.21666666666667, pntLon = -57.4, pntEle = Just 17.0, pntTime = Nothing}))
  , (CYDN, ICAOData (Point {pntLat = 51.1, pntLon = -100.05, pntEle = Just 305.0, pntTime = Nothing}))
  , (CYDP, ICAOData (Point {pntLat = 56.55, pntLon = -61.68333333333333, pntEle = Just 6.0, pntTime = Nothing}))
  , (CYED, ICAOData (Point {pntLat = 53.666666666666664, pntLon = -113.46666666666667, pntEle = Just 688.0, pntTime = Nothing}))
  , (CYEG, ICAOData (Point {pntLat = 53.3, pntLon = -113.58333333333333, pntEle = Just 715.0, pntTime = Nothing}))
  , (CYEN, ICAOData (Point {pntLat = 49.21666666666667, pntLon = -102.96666666666667, pntEle = Just 572.0, pntTime = Nothing}))
  , (CYET, ICAOData (Point {pntLat = 53.583333333333336, pntLon = -116.46666666666667, pntEle = Just 921.0, pntTime = Nothing}))
  , (CYEV, ICAOData (Point {pntLat = 68.3, pntLon = -133.48333333333332, pntEle = Just 59.0, pntTime = Nothing}))
  , (CYFB, ICAOData (Point {pntLat = 63.75, pntLon = -68.55, pntEle = Just 34.0, pntTime = Nothing}))
  , (CYFC, ICAOData (Point {pntLat = 45.86666666666667, pntLon = -66.53333333333333, pntEle = Just 17.0, pntTime = Nothing}))
  , (CYFS, ICAOData (Point {pntLat = 61.75, pntLon = -121.23333333333333, pntEle = Just 168.0, pntTime = Nothing}))
  , (CYGK, ICAOData (Point {pntLat = 44.21666666666667, pntLon = -76.6, pntEle = Just 93.0, pntTime = Nothing}))
  , (CYGL, ICAOData (Point {pntLat = 53.63333333333333, pntLon = -77.7, pntEle = Just 195.0, pntTime = Nothing}))
  , (CYGM, ICAOData (Point {pntLat = 50.61666666666667, pntLon = -97.03333333333333, pntEle = Just 230.0, pntTime = Nothing}))
  , (CYGP, ICAOData (Point {pntLat = 48.766666666666666, pntLon = -64.48333333333333, pntEle = Just 33.0, pntTime = Nothing}))
  , (CYGQ, ICAOData (Point {pntLat = 49.78333333333333, pntLon = -86.93333333333334, pntEle = Just 349.0, pntTime = Nothing}))
  , (CYGR, ICAOData (Point {pntLat = 47.416666666666664, pntLon = -61.78333333333333, pntEle = Just 10.0, pntTime = Nothing}))
  , (CYGV, ICAOData (Point {pntLat = 50.28333333333333, pntLon = -63.61666666666667, pntEle = Just 33.0, pntTime = Nothing}))
  , (CYGW, ICAOData (Point {pntLat = 55.28333333333333, pntLon = -77.76666666666667, pntEle = Just 21.0, pntTime = Nothing}))
  , (CYGX, ICAOData (Point {pntLat = 56.35, pntLon = -94.7, pntEle = Just 145.0, pntTime = Nothing}))
  , (CYHA, ICAOData (Point {pntLat = 61.05, pntLon = -69.63333333333334, pntEle = Just 30.0, pntTime = Nothing}))
  , (CYHB, ICAOData (Point {pntLat = 52.81666666666667, pntLon = -102.31666666666666, pntEle = Just 357.0, pntTime = Nothing}))
  , (CYHE, ICAOData (Point {pntLat = 49.36666666666667, pntLon = -121.48333333333333, pntEle = Just 39.0, pntTime = Nothing}))
  , (CYHM, ICAOData (Point {pntLat = 43.166666666666664, pntLon = -79.93333333333334, pntEle = Just 237.0, pntTime = Nothing}))
  , (CYHU, ICAOData (Point {pntLat = 45.516666666666666, pntLon = -73.41666666666667, pntEle = Just 27.0, pntTime = Nothing}))
  , (CYHY, ICAOData (Point {pntLat = 60.833333333333336, pntLon = -115.78333333333333, pntEle = Just 164.0, pntTime = Nothing}))
  , (CYHZ, ICAOData (Point {pntLat = 44.88333333333333, pntLon = -63.5, pntEle = Just 145.0, pntTime = Nothing}))
  , (CYIO, ICAOData (Point {pntLat = 72.7, pntLon = -77.96666666666667, pntEle = Just 59.0, pntTime = Nothing}))
  , (CYIV, ICAOData (Point {pntLat = 53.85, pntLon = -94.65, pntEle = Just 237.0, pntTime = Nothing}))
  , (CYJT, ICAOData (Point {pntLat = 48.53333333333333, pntLon = -58.55, pntEle = Just 8.0, pntTime = Nothing}))
  , (CYKA, ICAOData (Point {pntLat = 50.7, pntLon = -120.45, pntEle = Just 345.0, pntTime = Nothing}))
  , (CYKF, ICAOData (Point {pntLat = 43.46666666666667, pntLon = -80.38333333333334, pntEle = Just 317.0, pntTime = Nothing}))
  , (CYKJ, ICAOData (Point {pntLat = 57.25, pntLon = -105.61666666666666, pntEle = Just 509.0, pntTime = Nothing}))
  , (CYKL, ICAOData (Point {pntLat = 54.8, pntLon = -66.8, pntEle = Just 521.0, pntTime = Nothing}))
  , (CYKY, ICAOData (Point {pntLat = 51.516666666666666, pntLon = -109.16666666666667, pntEle = Just 694.0, pntTime = Nothing}))
  , (CYLJ, ICAOData (Point {pntLat = 54.13333333333333, pntLon = -108.51666666666667, pntEle = Just 480.0, pntTime = Nothing}))
  , (CYLL, ICAOData (Point {pntLat = 53.31666666666667, pntLon = -110.06666666666666, pntEle = Just 665.0, pntTime = Nothing}))
  , (CYLW, ICAOData (Point {pntLat = 49.96666666666667, pntLon = -119.38333333333334, pntEle = Just 430.0, pntTime = Nothing}))
  , (CYMA, ICAOData (Point {pntLat = 63.61666666666667, pntLon = -135.86666666666667, pntEle = Just 504.0, pntTime = Nothing}))
  , (CYMD, ICAOData (Point {pntLat = 76.23333333333333, pntLon = -119.33333333333333, pntEle = Just 15.0, pntTime = Nothing}))
  , (CYMJ, ICAOData (Point {pntLat = 50.333333333333336, pntLon = -105.55, pntEle = Just 577.0, pntTime = Nothing}))
  , (CYMM, ICAOData (Point {pntLat = 56.65, pntLon = -111.21666666666667, pntEle = Just 369.0, pntTime = Nothing}))
  , (CYMO, ICAOData (Point {pntLat = 51.266666666666666, pntLon = -80.65, pntEle = Just 10.0, pntTime = Nothing}))
  , (CYMT, ICAOData (Point {pntLat = 49.766666666666666, pntLon = -74.53333333333333, pntEle = Just 388.0, pntTime = Nothing}))
  , (CYNA, ICAOData (Point {pntLat = 50.18333333333333, pntLon = -61.81666666666667, pntEle = Just 7.0, pntTime = Nothing}))
  , (CYNE, ICAOData (Point {pntLat = 53.96666666666667, pntLon = -97.83333333333333, pntEle = Just 223.0, pntTime = Nothing}))
  , (CYNM, ICAOData (Point {pntLat = 49.766666666666666, pntLon = -77.81666666666666, pntEle = Just 281.0, pntTime = Nothing}))
  , (CYOD, ICAOData (Point {pntLat = 54.416666666666664, pntLon = -110.28333333333333, pntEle = Just 541.0, pntTime = Nothing}))
  , (CYOJ, ICAOData (Point {pntLat = 58.61666666666667, pntLon = -117.16666666666667, pntEle = Just 338.0, pntTime = Nothing}))
  , (CYOW, ICAOData (Point {pntLat = 45.31666666666667, pntLon = -75.66666666666667, pntEle = Just 116.0, pntTime = Nothing}))
  , (CYOY, ICAOData (Point {pntLat = 46.9, pntLon = -71.5, pntEle = Just 168.0, pntTime = Nothing}))
  , (CYPA, ICAOData (Point {pntLat = 53.21666666666667, pntLon = -105.68333333333334, pntEle = Just 428.0, pntTime = Nothing}))
  , (CYPE, ICAOData (Point {pntLat = 56.233333333333334, pntLon = -117.43333333333334, pntEle = Just 571.0, pntTime = Nothing}))
  , (CYPG, ICAOData (Point {pntLat = 49.9, pntLon = -98.26666666666667, pntEle = Just 269.0, pntTime = Nothing}))
  , (CYPQ, ICAOData (Point {pntLat = 44.233333333333334, pntLon = -78.36666666666666, pntEle = Just 191.0, pntTime = Nothing}))
  , (CYPR, ICAOData (Point {pntLat = 54.3, pntLon = -130.43333333333334, pntEle = Just 34.0, pntTime = Nothing}))
  , (CYPW, ICAOData (Point {pntLat = 49.833333333333336, pntLon = -124.5, pntEle = Just 130.0, pntTime = Nothing}))
  , (CYPY, ICAOData (Point {pntLat = 58.766666666666666, pntLon = -111.11666666666666, pntEle = Just 232.0, pntTime = Nothing}))
  , (CYPZ, ICAOData (Point {pntLat = 54.38333333333333, pntLon = -125.95, pntEle = Just 713.0, pntTime = Nothing}))
  , (CYQA, ICAOData (Point {pntLat = 44.96666666666667, pntLon = -79.3, pntEle = Just 280.0, pntTime = Nothing}))
  , (CYQB, ICAOData (Point {pntLat = 46.8, pntLon = -71.38333333333334, pntEle = Just 70.0, pntTime = Nothing}))
  , (CYQD, ICAOData (Point {pntLat = 53.96666666666667, pntLon = -101.1, pntEle = Just 271.0, pntTime = Nothing}))
  , (CYQF, ICAOData (Point {pntLat = 52.18333333333333, pntLon = -113.9, pntEle = Just 905.0, pntTime = Nothing}))
  , (CYQG, ICAOData (Point {pntLat = 42.266666666666666, pntLon = -82.96666666666667, pntEle = Just 190.0, pntTime = Nothing}))
  , (CYQH, ICAOData (Point {pntLat = 60.11666666666667, pntLon = -128.81666666666666, pntEle = Just 690.0, pntTime = Nothing}))
  , (CYQI, ICAOData (Point {pntLat = 43.833333333333336, pntLon = -66.08333333333333, pntEle = Just 43.0, pntTime = Nothing}))
  , (CYQK, ICAOData (Point {pntLat = 49.78333333333333, pntLon = -94.36666666666666, pntEle = Just 407.0, pntTime = Nothing}))
  , (CYQL, ICAOData (Point {pntLat = 49.63333333333333, pntLon = -112.8, pntEle = Just 929.0, pntTime = Nothing}))
  , (CYQM, ICAOData (Point {pntLat = 46.11666666666667, pntLon = -64.68333333333334, pntEle = Just 71.0, pntTime = Nothing}))
  , (CYQQ, ICAOData (Point {pntLat = 49.71666666666667, pntLon = -124.9, pntEle = Just 24.0, pntTime = Nothing}))
  , (CYQR, ICAOData (Point {pntLat = 50.43333333333333, pntLon = -104.66666666666667, pntEle = Just 577.0, pntTime = Nothing}))
  , (CYQT, ICAOData (Point {pntLat = 48.36666666666667, pntLon = -89.31666666666666, pntEle = Just 199.0, pntTime = Nothing}))
  , (CYQU, ICAOData (Point {pntLat = 55.18333333333333, pntLon = -118.88333333333334, pntEle = Just 666.0, pntTime = Nothing}))
  , (CYQV, ICAOData (Point {pntLat = 51.266666666666666, pntLon = -102.46666666666667, pntEle = Just 498.0, pntTime = Nothing}))
  , (CYQW, ICAOData (Point {pntLat = 52.766666666666666, pntLon = -108.25, pntEle = Just 548.0, pntTime = Nothing}))
  , (CYQX, ICAOData (Point {pntLat = 48.95, pntLon = -54.56666666666667, pntEle = Just 151.0, pntTime = Nothing}))
  , (CYQY, ICAOData (Point {pntLat = 46.166666666666664, pntLon = -60.05, pntEle = Just 56.0, pntTime = Nothing}))
  , (CYQZ, ICAOData (Point {pntLat = 53.03333333333333, pntLon = -122.51666666666667, pntEle = Just 545.0, pntTime = Nothing}))
  , (CYRB, ICAOData (Point {pntLat = 74.71666666666667, pntLon = -94.98333333333333, pntEle = Just 67.0, pntTime = Nothing}))
  , (CYRJ, ICAOData (Point {pntLat = 48.516666666666666, pntLon = -72.26666666666667, pntEle = Just 179.0, pntTime = Nothing}))
  , (CYRL, ICAOData (Point {pntLat = 51.06666666666667, pntLon = -93.8, pntEle = Just 375.0, pntTime = Nothing}))
  , (CYRM, ICAOData (Point {pntLat = 52.43333333333333, pntLon = -114.91666666666667, pntEle = Just 988.0, pntTime = Nothing}))
  , (CYRT, ICAOData (Point {pntLat = 62.81666666666667, pntLon = -92.11666666666666, pntEle = Just 31.0, pntTime = Nothing}))
  , (CYRV, ICAOData (Point {pntLat = 50.96666666666667, pntLon = -118.18333333333334, pntEle = Just 443.0, pntTime = Nothing}))
  , (CYSB, ICAOData (Point {pntLat = 46.61666666666667, pntLon = -80.8, pntEle = Just 348.0, pntTime = Nothing}))
  , (CYSC, ICAOData (Point {pntLat = 45.4, pntLon = -71.88333333333334, pntEle = Just 170.0, pntTime = Nothing}))
  , (CYSD, ICAOData (Point {pntLat = 50.266666666666666, pntLon = -111.18333333333334, pntEle = Just 770.0, pntTime = Nothing}))
  , (CYSF, ICAOData (Point {pntLat = 59.25, pntLon = -105.83333333333333, pntEle = Just 250.0, pntTime = Nothing}))
  , (CYSJ, ICAOData (Point {pntLat = 45.333333333333336, pntLon = -65.88333333333334, pntEle = Just 103.0, pntTime = Nothing}))
  , (CYSL, ICAOData (Point {pntLat = 47.15, pntLon = -67.83333333333333, pntEle = Just 241.0, pntTime = Nothing}))
  , (CYSM, ICAOData (Point {pntLat = 60.016666666666666, pntLon = -111.95, pntEle = Just 203.0, pntTime = Nothing}))
  , (CYSU, ICAOData (Point {pntLat = 46.43333333333333, pntLon = -63.833333333333336, pntEle = Just 24.0, pntTime = Nothing}))
  , (CYTE, ICAOData (Point {pntLat = 64.21666666666667, pntLon = -76.53333333333333, pntEle = Just 51.0, pntTime = Nothing}))
  , (CYTH, ICAOData (Point {pntLat = 55.8, pntLon = -97.85, pntEle = Just 204.0, pntTime = Nothing}))
  , (CYTL, ICAOData (Point {pntLat = 53.833333333333336, pntLon = -89.86666666666666, pntEle = Just 224.0, pntTime = Nothing}))
  , (CYTR, ICAOData (Point {pntLat = 44.11666666666667, pntLon = -77.53333333333333, pntEle = Just 85.0, pntTime = Nothing}))
  , (CYTS, ICAOData (Point {pntLat = 48.56666666666667, pntLon = -81.36666666666666, pntEle = Just 295.0, pntTime = Nothing}))
  , (CYTZ, ICAOData (Point {pntLat = 43.63333333333333, pntLon = -79.4, pntEle = Just 77.0, pntTime = Nothing}))
  , (CYUA, ICAOData (Point {pntLat = 68.95, pntLon = -137.21666666666667, pntEle = Just 56.0, pntTime = Nothing}))
  , (CYUB, ICAOData (Point {pntLat = 69.45, pntLon = -133.01666666666668, pntEle = Just 5.0, pntTime = Nothing}))
  , (CYUI, ICAOData (Point {pntLat = 68.93333333333334, pntLon = -116.91666666666667, pntEle = Just 16.0, pntTime = Nothing}))
  , (CYUJ, ICAOData (Point {pntLat = 68.5, pntLon = -113.21666666666667, pntEle = Just 21.0, pntTime = Nothing}))
  , (CYUK, ICAOData (Point {pntLat = 68.75, pntLon = -109.06666666666666, pntEle = Just 92.0, pntTime = Nothing}))
  , (CYUL, ICAOData (Point {pntLat = 45.46666666666667, pntLon = -73.75, pntEle = Just 31.0, pntTime = Nothing}))
  , (CYUQ, ICAOData (Point {pntLat = 68.65, pntLon = -101.73333333333333, pntEle = Just 18.0, pntTime = Nothing}))
  , (CYUS, ICAOData (Point {pntLat = 68.81666666666666, pntLon = -93.43333333333334, pntEle = Just 51.0, pntTime = Nothing}))
  , (CYUX, ICAOData (Point {pntLat = 68.78333333333333, pntLon = -81.25, pntEle = Just 8.0, pntTime = Nothing}))
  , (CYVC, ICAOData (Point {pntLat = 55.15, pntLon = -105.26666666666667, pntEle = Just 375.0, pntTime = Nothing}))
  , (CYVO, ICAOData (Point {pntLat = 48.06666666666667, pntLon = -77.78333333333333, pntEle = Just 337.0, pntTime = Nothing}))
  , (CYVP, ICAOData (Point {pntLat = 58.1, pntLon = -68.41666666666667, pntEle = Just 37.0, pntTime = Nothing}))
  , (CYVQ, ICAOData (Point {pntLat = 65.28333333333333, pntLon = -126.8, pntEle = Just 67.0, pntTime = Nothing}))
  , (CYVR, ICAOData (Point {pntLat = 49.18333333333333, pntLon = -123.16666666666667, pntEle = Just 3.0, pntTime = Nothing}))
  , (CYVV, ICAOData (Point {pntLat = 44.75, pntLon = -81.1, pntEle = Just 222.0, pntTime = Nothing}))
  , (CYWA, ICAOData (Point {pntLat = 45.95, pntLon = -77.31666666666666, pntEle = Just 130.0, pntTime = Nothing}))
  , (CYWG, ICAOData (Point {pntLat = 49.9, pntLon = -97.23333333333333, pntEle = Just 239.0, pntTime = Nothing}))
  , (CYWK, ICAOData (Point {pntLat = 52.93333333333333, pntLon = -66.86666666666666, pntEle = Just 551.0, pntTime = Nothing}))
  , (CYWL, ICAOData (Point {pntLat = 52.18333333333333, pntLon = -122.05, pntEle = Just 940.0, pntTime = Nothing}))
  , (CYXC, ICAOData (Point {pntLat = 49.61666666666667, pntLon = -115.78333333333333, pntEle = Just 939.0, pntTime = Nothing}))
  , (CYXD, ICAOData (Point {pntLat = 53.56666666666667, pntLon = -113.51666666666667, pntEle = Just 671.0, pntTime = Nothing}))
  , (CYXE, ICAOData (Point {pntLat = 52.166666666666664, pntLon = -106.68333333333334, pntEle = Just 501.0, pntTime = Nothing}))
  , (CYXH, ICAOData (Point {pntLat = 50.016666666666666, pntLon = -110.71666666666667, pntEle = Just 717.0, pntTime = Nothing}))
  , (CYXJ, ICAOData (Point {pntLat = 56.233333333333334, pntLon = -120.73333333333333, pntEle = Just 695.0, pntTime = Nothing}))
  , (CYXL, ICAOData (Point {pntLat = 50.11666666666667, pntLon = -91.9, pntEle = Just 389.0, pntTime = Nothing}))
  , (CYXR, ICAOData (Point {pntLat = 47.7, pntLon = -79.85, pntEle = Just 243.0, pntTime = Nothing}))
  , (CYXS, ICAOData (Point {pntLat = 53.88333333333333, pntLon = -122.68333333333334, pntEle = Just 691.0, pntTime = Nothing}))
  , (CYXT, ICAOData (Point {pntLat = 54.46666666666667, pntLon = -128.58333333333334, pntEle = Just 217.0, pntTime = Nothing}))
  , (CYXU, ICAOData (Point {pntLat = 43.03333333333333, pntLon = -81.15, pntEle = Just 278.0, pntTime = Nothing}))
  , (CYXX, ICAOData (Point {pntLat = 49.03333333333333, pntLon = -122.36666666666666, pntEle = Just 54.0, pntTime = Nothing}))
  , (CYXY, ICAOData (Point {pntLat = 60.71666666666667, pntLon = -135.06666666666666, pntEle = Just 703.0, pntTime = Nothing}))
  , (CYXZ, ICAOData (Point {pntLat = 47.96666666666667, pntLon = -84.78333333333333, pntEle = Just 287.0, pntTime = Nothing}))
  , (CYYB, ICAOData (Point {pntLat = 46.35, pntLon = -79.43333333333334, pntEle = Just 358.0, pntTime = Nothing}))
  , (CYYC, ICAOData (Point {pntLat = 51.11666666666667, pntLon = -114.01666666666667, pntEle = Just 1077.0, pntTime = Nothing}))
  , (CYYD, ICAOData (Point {pntLat = 54.81666666666667, pntLon = -127.18333333333334, pntEle = Just 523.0, pntTime = Nothing}))
  , (CYYE, ICAOData (Point {pntLat = 58.833333333333336, pntLon = -122.58333333333333, pntEle = Just 382.0, pntTime = Nothing}))
  , (CYYF, ICAOData (Point {pntLat = 49.46666666666667, pntLon = -119.6, pntEle = Just 344.0, pntTime = Nothing}))
  , (CYYG, ICAOData (Point {pntLat = 46.28333333333333, pntLon = -63.13333333333333, pntEle = Just 48.0, pntTime = Nothing}))
  , (CYYJ, ICAOData (Point {pntLat = 48.65, pntLon = -123.43333333333334, pntEle = Just 20.0, pntTime = Nothing}))
  , (CYYL, ICAOData (Point {pntLat = 56.86666666666667, pntLon = -101.08333333333333, pntEle = Just 357.0, pntTime = Nothing}))
  , (CYYN, ICAOData (Point {pntLat = 50.28333333333333, pntLon = -107.68333333333334, pntEle = Just 818.0, pntTime = Nothing}))
  , (CYYQ, ICAOData (Point {pntLat = 58.733333333333334, pntLon = -94.06666666666666, pntEle = Just 28.0, pntTime = Nothing}))
  , (CYYR, ICAOData (Point {pntLat = 53.31666666666667, pntLon = -60.416666666666664, pntEle = Just 46.0, pntTime = Nothing}))
  , (CYYT, ICAOData (Point {pntLat = 47.61666666666667, pntLon = -52.733333333333334, pntEle = Just 134.0, pntTime = Nothing}))
  , (CYYU, ICAOData (Point {pntLat = 49.416666666666664, pntLon = -82.46666666666667, pntEle = Just 227.0, pntTime = Nothing}))
  , (CYYW, ICAOData (Point {pntLat = 50.3, pntLon = -89.03333333333333, pntEle = Just 351.0, pntTime = Nothing}))
  , (CYYY, ICAOData (Point {pntLat = 48.6, pntLon = -68.21666666666667, pntEle = Just 48.0, pntTime = Nothing}))
  , (CYYZ, ICAOData (Point {pntLat = 43.666666666666664, pntLon = -79.63333333333334, pntEle = Just 173.0, pntTime = Nothing}))
  , (CYZE, ICAOData (Point {pntLat = 45.88333333333333, pntLon = -82.56666666666666, pntEle = Just 193.0, pntTime = Nothing}))
  , (CYZF, ICAOData (Point {pntLat = 62.46666666666667, pntLon = -114.45, pntEle = Just 205.0, pntTime = Nothing}))
  , (CYZH, ICAOData (Point {pntLat = 55.3, pntLon = -114.78333333333333, pntEle = Just 581.0, pntTime = Nothing}))
  , (CYZP, ICAOData (Point {pntLat = 53.25, pntLon = -131.81666666666666, pntEle = Just 6.0, pntTime = Nothing}))
  , (CYZR, ICAOData (Point {pntLat = 43.0, pntLon = -82.31666666666666, pntEle = Just 181.0, pntTime = Nothing}))
  , (CYZS, ICAOData (Point {pntLat = 64.2, pntLon = -83.36666666666666, pntEle = Just 64.0, pntTime = Nothing}))
  , (CYZT, ICAOData (Point {pntLat = 50.68333333333333, pntLon = -127.36666666666666, pntEle = Just 22.0, pntTime = Nothing}))
  , (CYZU, ICAOData (Point {pntLat = 54.15, pntLon = -115.78333333333333, pntEle = Just 783.0, pntTime = Nothing}))
  , (CYZV, ICAOData (Point {pntLat = 50.21666666666667, pntLon = -66.26666666666667, pntEle = Just 55.0, pntTime = Nothing}))
  , (CYZW, ICAOData (Point {pntLat = 60.166666666666664, pntLon = -132.73333333333332, pntEle = Just 711.0, pntTime = Nothing}))
  , (CYZX, ICAOData (Point {pntLat = 44.983333333333334, pntLon = -64.91666666666667, pntEle = Just 28.0, pntTime = Nothing}))
  , (CYZY, ICAOData (Point {pntLat = 55.3, pntLon = -123.13333333333334, pntEle = Just 695.0, pntTime = Nothing}))
  , (CZDI, ICAOData (Point {pntLat = 53.56666666666667, pntLon = -64.1, pntEle = Just 440.0, pntTime = Nothing}))
  , (CZOC, ICAOData (Point {pntLat = 67.56666666666666, pntLon = -139.83333333333334, pntEle = Just 251.0, pntTime = Nothing}))
  , (CZPK, ICAOData (Point {pntLat = 69.35, pntLon = -124.05, pntEle = Just 6.0, pntTime = Nothing}))
  , (CZST, ICAOData (Point {pntLat = 55.93333333333333, pntLon = -129.98333333333332, pntEle = Just 7.0, pntTime = Nothing}))
  , (CZUB, ICAOData (Point {pntLat = 69.43333333333334, pntLon = -133.03333333333333, pntEle = Just 6.0, pntTime = Nothing}))
  , (CZUE, ICAOData (Point {pntLat = 70.16666666666667, pntLon = -124.7, pntEle = Just 17.0, pntTime = Nothing}))
  , (CZUM, ICAOData (Point {pntLat = 53.55, pntLon = -64.1, pntEle = Just 440.0, pntTime = Nothing}))
  , (DAAD, ICAOData (Point {pntLat = 35.333333333333336, pntLon = 4.2, pntEle = Just 461.0, pntTime = Nothing}))
  , (DAAE, ICAOData (Point {pntLat = 36.71666666666667, pntLon = 5.066666666666666, pntEle = Just 2.0, pntTime = Nothing}))
  , (DAAG, ICAOData (Point {pntLat = 36.71666666666667, pntLon = 3.25, pntEle = Just 25.0, pntTime = Nothing}))
  , (DAAJ, ICAOData (Point {pntLat = 24.55, pntLon = 9.466666666666667, pntEle = Just 1054.0, pntTime = Nothing}))
  , (DAAP, ICAOData (Point {pntLat = 26.5, pntLon = 8.416666666666666, pntEle = Just 558.0, pntTime = Nothing}))
  , (DAAS, ICAOData (Point {pntLat = 36.18333333333333, pntLon = 5.416666666666667, pntEle = Just 1038.0, pntTime = Nothing}))
  , (DAAV, ICAOData (Point {pntLat = 36.88333333333333, pntLon = 5.816666666666666, pntEle = Just 2.0, pntTime = Nothing}))
  , (DAAY, ICAOData (Point {pntLat = 34.93333333333333, pntLon = -0.43333333333333335, pntEle = Just 1149.0, pntTime = Nothing}))
  , (DABB, ICAOData (Point {pntLat = 36.833333333333336, pntLon = 7.816666666666666, pntEle = Just 4.0, pntTime = Nothing}))
  , (DABC, ICAOData (Point {pntLat = 36.28333333333333, pntLon = 6.616666666666667, pntEle = Just 694.0, pntTime = Nothing}))
  , (DABP, ICAOData (Point {pntLat = 36.93333333333333, pntLon = 6.95, pntEle = Just 7.0, pntTime = Nothing}))
  , (DABS, ICAOData (Point {pntLat = 35.483333333333334, pntLon = 8.133333333333333, pntEle = Just 813.0, pntTime = Nothing}))
  , (DABT, ICAOData (Point {pntLat = 35.55, pntLon = 6.183333333333334, pntEle = Just 1052.0, pntTime = Nothing}))
  , (DAFI, ICAOData (Point {pntLat = 34.68333333333333, pntLon = 3.25, pntEle = Just 1144.0, pntTime = Nothing}))
  , (DAOB, ICAOData (Point {pntLat = 35.25, pntLon = 1.4333333333333333, pntEle = Just 1127.0, pntTime = Nothing}))
  , (DAOF, ICAOData (Point {pntLat = 27.666666666666668, pntLon = -8.133333333333333, pntEle = Just 431.0, pntTime = Nothing}))
  , (DAOI, ICAOData (Point {pntLat = 36.21666666666667, pntLon = 1.3333333333333333, pntEle = Just 143.0, pntTime = Nothing}))
  , (DAON, ICAOData (Point {pntLat = 35.016666666666666, pntLon = -1.4666666666666668, pntEle = Just 247.0, pntTime = Nothing}))
  , (DAOO, ICAOData (Point {pntLat = 35.63333333333333, pntLon = -0.6, pntEle = Just 90.0, pntTime = Nothing}))
  , (DAOR, ICAOData (Point {pntLat = 31.616666666666667, pntLon = -2.2333333333333334, pntEle = Just 773.0, pntTime = Nothing}))
  , (DAOV, ICAOData (Point {pntLat = 35.2, pntLon = 0.13333333333333333, pntEle = Just 514.0, pntTime = Nothing}))
  , (DAUA, ICAOData (Point {pntLat = 27.883333333333333, pntLon = -0.2833333333333333, pntEle = Just 263.0, pntTime = Nothing}))
  , (DAUB, ICAOData (Point {pntLat = 34.8, pntLon = 5.733333333333333, pntEle = Just 87.0, pntTime = Nothing}))
  , (DAUE, ICAOData (Point {pntLat = 30.566666666666666, pntLon = 2.8666666666666667, pntEle = Just 397.0, pntTime = Nothing}))
  , (DAUG, ICAOData (Point {pntLat = 32.38333333333333, pntLon = 3.8166666666666664, pntEle = Just 450.0, pntTime = Nothing}))
  , (DAUH, ICAOData (Point {pntLat = 31.666666666666668, pntLon = 6.15, pntEle = Just 142.0, pntTime = Nothing}))
  , (DAUK, ICAOData (Point {pntLat = 33.11666666666667, pntLon = 6.133333333333334, pntEle = Just 85.0, pntTime = Nothing}))
  , (DAUL, ICAOData (Point {pntLat = 33.766666666666666, pntLon = 2.9333333333333336, pntEle = Just 765.0, pntTime = Nothing}))
  , (DAUO, ICAOData (Point {pntLat = 33.5, pntLon = 6.116666666666666, pntEle = Just 63.0, pntTime = Nothing}))
  , (DAUT, ICAOData (Point {pntLat = 29.25, pntLon = 0.2833333333333333, pntEle = Just 312.0, pntTime = Nothing}))
  , (DAUU, ICAOData (Point {pntLat = 31.916666666666668, pntLon = 5.4, pntEle = Just 141.0, pntTime = Nothing}))
  , (DAUZ, ICAOData (Point {pntLat = 28.05, pntLon = 9.633333333333333, pntEle = Just 562.0, pntTime = Nothing}))
  , (DBBB, ICAOData (Point {pntLat = 6.35, pntLon = 2.3833333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (DBBC, ICAOData (Point {pntLat = 7.166666666666667, pntLon = 2.066666666666667, pntEle = Just 166.0, pntTime = Nothing}))
  , (DBBK, ICAOData (Point {pntLat = 11.133333333333333, pntLon = 2.9333333333333336, pntEle = Just 290.0, pntTime = Nothing}))
  , (DBBN, ICAOData (Point {pntLat = 10.316666666666666, pntLon = 1.3833333333333333, pntEle = Just 460.0, pntTime = Nothing}))
  , (DBBP, ICAOData (Point {pntLat = 9.35, pntLon = 2.6166666666666667, pntEle = Just 392.0, pntTime = Nothing}))
  , (DBBS, ICAOData (Point {pntLat = 8.033333333333333, pntLon = 2.466666666666667, pntEle = Just 199.0, pntTime = Nothing}))
  , (DFCC, ICAOData (Point {pntLat = 13.566666666666666, pntLon = -2.4166666666666665, pntEle = Just 337.0, pntTime = Nothing}))
  , (DFCO, ICAOData (Point {pntLat = 11.75, pntLon = -2.9333333333333336, pntEle = Just 270.0, pntTime = Nothing}))
  , (DFCP, ICAOData (Point {pntLat = 11.15, pntLon = -1.15, pntEle = Just 320.0, pntTime = Nothing}))
  , (DFEE, ICAOData (Point {pntLat = 14.033333333333333, pntLon = -3.333333333333333e-2, pntEle = Just 276.0, pntTime = Nothing}))
  , (DFEF, ICAOData (Point {pntLat = 12.033333333333333, pntLon = 0.36666666666666664, pntEle = Just 308.0, pntTime = Nothing}))
  , (DFFD, ICAOData (Point {pntLat = 12.35, pntLon = -1.5166666666666666, pntEle = Just 316.0, pntTime = Nothing}))
  , (DFOD, ICAOData (Point {pntLat = 12.466666666666667, pntLon = -3.4833333333333334, pntEle = Just 300.0, pntTime = Nothing}))
  , (DFOG, ICAOData (Point {pntLat = 10.333333333333334, pntLon = -3.183333333333333, pntEle = Just 333.0, pntTime = Nothing}))
  , (DFOO, ICAOData (Point {pntLat = 11.166666666666666, pntLon = -4.316666666666666, pntEle = Just 460.0, pntTime = Nothing}))
  , (DGAA, ICAOData (Point {pntLat = 5.6, pntLon = -0.16666666666666666, pntEle = Just 68.0, pntTime = Nothing}))
  , (DGAD, ICAOData (Point {pntLat = 5.783333333333333, pntLon = 0.6333333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (DGAH, ICAOData (Point {pntLat = 6.6, pntLon = 0.4666666666666667, pntEle = Just 158.0, pntTime = Nothing}))
  , (DGAK, ICAOData (Point {pntLat = 6.1, pntLon = 0.11666666666666667, pntEle = Just 17.0, pntTime = Nothing}))
  , (DGAS, ICAOData (Point {pntLat = 5.2, pntLon = -1.0666666666666667, pntEle = Just 44.0, pntTime = Nothing}))
  , (DGAT, ICAOData (Point {pntLat = 5.616666666666667, pntLon = 0.0, pntEle = Just 14.0, pntTime = Nothing}))
  , (DGKA, ICAOData (Point {pntLat = 5.933333333333334, pntLon = -0.9833333333333333, pntEle = Just 139.0, pntTime = Nothing}))
  , (DGKK, ICAOData (Point {pntLat = 6.083333333333333, pntLon = -0.25, pntEle = Just 166.0, pntTime = Nothing}))
  , (DGLB, ICAOData (Point {pntLat = 9.033333333333333, pntLon = -2.4833333333333334, pntEle = Just 299.0, pntTime = Nothing}))
  , (DGLE, ICAOData (Point {pntLat = 9.5, pntLon = -0.85, pntEle = Just 168.0, pntTime = Nothing}))
  , (DGLN, ICAOData (Point {pntLat = 10.9, pntLon = -1.1, pntEle = Just 201.0, pntTime = Nothing}))
  , (DGLW, ICAOData (Point {pntLat = 10.05, pntLon = -2.5, pntEle = Just 323.0, pntTime = Nothing}))
  , (DGLY, ICAOData (Point {pntLat = 9.45, pntLon = -1.6666666666666666e-2, pntEle = Just 195.0, pntTime = Nothing}))
  , (DGSB, ICAOData (Point {pntLat = 6.2, pntLon = -2.3333333333333335, pntEle = Just 171.0, pntTime = Nothing}))
  , (DGSI, ICAOData (Point {pntLat = 6.716666666666667, pntLon = -1.6, pntEle = Just 287.0, pntTime = Nothing}))
  , (DGSN, ICAOData (Point {pntLat = 7.333333333333333, pntLon = -2.3333333333333335, pntEle = Just 309.0, pntTime = Nothing}))
  , (DGSW, ICAOData (Point {pntLat = 7.75, pntLon = -2.1, pntEle = Just 339.0, pntTime = Nothing}))
  , (DGTK, ICAOData (Point {pntLat = 4.883333333333333, pntLon = -1.7666666666666666, pntEle = Just 5.0, pntTime = Nothing}))
  , (DGTX, ICAOData (Point {pntLat = 4.866666666666667, pntLon = -2.2333333333333334, pntEle = Just 38.0, pntTime = Nothing}))
  , (DIAD, ICAOData (Point {pntLat = 5.3, pntLon = -3.3, pntEle = Just 33.0, pntTime = Nothing}))
  , (DIAP, ICAOData (Point {pntLat = 5.25, pntLon = -3.9333333333333336, pntEle = Just 7.0, pntTime = Nothing}))
  , (DIBK, ICAOData (Point {pntLat = 7.733333333333333, pntLon = -5.066666666666666, pntEle = Just 376.0, pntTime = Nothing}))
  , (DIBU, ICAOData (Point {pntLat = 8.05, pntLon = -2.783333333333333, pntEle = Just 369.0, pntTime = Nothing}))
  , (DIDK, ICAOData (Point {pntLat = 6.65, pntLon = -4.7, pntEle = Just 92.0, pntTime = Nothing}))
  , (DIDL, ICAOData (Point {pntLat = 6.866666666666667, pntLon = -6.466666666666667, pntEle = Just 276.0, pntTime = Nothing}))
  , (DIGA, ICAOData (Point {pntLat = 6.133333333333334, pntLon = -5.95, pntEle = Just 205.0, pntTime = Nothing}))
  , (DIKO, ICAOData (Point {pntLat = 9.416666666666666, pntLon = -5.616666666666667, pntEle = Just 381.0, pntTime = Nothing}))
  , (DIMN, ICAOData (Point {pntLat = 7.383333333333334, pntLon = -7.516666666666667, pntEle = Just 339.0, pntTime = Nothing}))
  , (DIOD, ICAOData (Point {pntLat = 9.5, pntLon = -7.566666666666666, pntEle = Just 434.0, pntTime = Nothing}))
  , (DISP, ICAOData (Point {pntLat = 4.75, pntLon = -6.65, pntEle = Just 30.0, pntTime = Nothing}))
  , (DISS, ICAOData (Point {pntLat = 4.95, pntLon = -6.083333333333333, pntEle = Just 62.0, pntTime = Nothing}))
  , (DITB, ICAOData (Point {pntLat = 4.416666666666667, pntLon = -7.366666666666666, pntEle = Just 21.0, pntTime = Nothing}))
  , (DIYO, ICAOData (Point {pntLat = 6.9, pntLon = -5.35, pntEle = Just 196.0, pntTime = Nothing}))
  , (DNBI, ICAOData (Point {pntLat = 9.1, pntLon = 6.016666666666667, pntEle = Just 137.0, pntTime = Nothing}))
  , (DNCA, ICAOData (Point {pntLat = 4.966666666666667, pntLon = 8.35, pntEle = Just 63.0, pntTime = Nothing}))
  , (DNEN, ICAOData (Point {pntLat = 6.466666666666667, pntLon = 7.55, pntEle = Just 137.0, pntTime = Nothing}))
  , (DNGU, ICAOData (Point {pntLat = 12.166666666666666, pntLon = 6.7, pntEle = Just 463.0, pntTime = Nothing}))
  , (DNIB, ICAOData (Point {pntLat = 7.433333333333334, pntLon = 3.9, pntEle = Just 234.0, pntTime = Nothing}))
  , (DNIL, ICAOData (Point {pntLat = 8.483333333333333, pntLon = 4.583333333333333, pntEle = Just 308.0, pntTime = Nothing}))
  , (DNJO, ICAOData (Point {pntLat = 9.866666666666667, pntLon = 8.9, pntEle = Just 1295.0, pntTime = Nothing}))
  , (DNKA, ICAOData (Point {pntLat = 10.6, pntLon = 7.45, pntEle = Just 645.0, pntTime = Nothing}))
  , (DNKN, ICAOData (Point {pntLat = 12.05, pntLon = 8.533333333333333, pntEle = Just 476.0, pntTime = Nothing}))
  , (DNMA, ICAOData (Point {pntLat = 11.85, pntLon = 13.083333333333334, pntEle = Just 354.0, pntTime = Nothing}))
  , (DNMK, ICAOData (Point {pntLat = 7.683333333333334, pntLon = 8.616666666666667, pntEle = Just 113.0, pntTime = Nothing}))
  , (DNMM, ICAOData (Point {pntLat = 6.583333333333333, pntLon = 3.3333333333333335, pntEle = Just 40.0, pntTime = Nothing}))
  , (DNOS, ICAOData (Point {pntLat = 7.783333333333333, pntLon = 4.483333333333333, pntEle = Just 305.0, pntTime = Nothing}))
  , (DNPO, ICAOData (Point {pntLat = 4.85, pntLon = 7.016666666666667, pntEle = Just 18.0, pntTime = Nothing}))
  , (DNSO, ICAOData (Point {pntLat = 13.016666666666667, pntLon = 5.25, pntEle = Just 351.0, pntTime = Nothing}))
  , (DNYO, ICAOData (Point {pntLat = 9.233333333333333, pntLon = 12.466666666666667, pntEle = Just 191.0, pntTime = Nothing}))
  , (DNZA, ICAOData (Point {pntLat = 11.133333333333333, pntLon = 7.683333333333334, pntEle = Just 664.0, pntTime = Nothing}))
  , (DRRB, ICAOData (Point {pntLat = 13.8, pntLon = 5.25, pntEle = Just 272.0, pntTime = Nothing}))
  , (DRRG, ICAOData (Point {pntLat = 11.883333333333333, pntLon = 3.45, pntEle = Just 202.0, pntTime = Nothing}))
  , (DRRI, ICAOData (Point {pntLat = 18.683333333333334, pntLon = 12.916666666666666, pntEle = Just 355.0, pntTime = Nothing}))
  , (DRRL, ICAOData (Point {pntLat = 14.2, pntLon = 1.45, pntEle = Just 209.0, pntTime = Nothing}))
  , (DRRM, ICAOData (Point {pntLat = 13.466666666666667, pntLon = 7.083333333333333, pntEle = Just 372.0, pntTime = Nothing}))
  , (DRRN, ICAOData (Point {pntLat = 13.483333333333333, pntLon = 2.1666666666666665, pntEle = Just 223.0, pntTime = Nothing}))
  , (DRRT, ICAOData (Point {pntLat = 14.9, pntLon = 5.25, pntEle = Just 386.0, pntTime = Nothing}))
  , (DRZA, ICAOData (Point {pntLat = 16.966666666666665, pntLon = 7.983333333333333, pntEle = Just 501.0, pntTime = Nothing}))
  , (DRZF, ICAOData (Point {pntLat = 13.416666666666666, pntLon = 12.783333333333333, pntEle = Just 303.0, pntTime = Nothing}))
  , (DRZG, ICAOData (Point {pntLat = 13.983333333333333, pntLon = 10.3, pntEle = Just 464.0, pntTime = Nothing}))
  , (DRZM, ICAOData (Point {pntLat = 13.233333333333333, pntLon = 11.983333333333333, pntEle = Just 338.0, pntTime = Nothing}))
  , (DRZR, ICAOData (Point {pntLat = 13.783333333333333, pntLon = 8.983333333333333, pntEle = Just 452.0, pntTime = Nothing}))
  , (DTKA, ICAOData (Point {pntLat = 36.95, pntLon = 8.75, pntEle = Just 20.0, pntTime = Nothing}))
  , (DTTA, ICAOData (Point {pntLat = 36.833333333333336, pntLon = 10.233333333333333, pntEle = Just 3.0, pntTime = Nothing}))
  , (DTTB, ICAOData (Point {pntLat = 37.25, pntLon = 9.8, pntEle = Just 5.0, pntTime = Nothing}))
  , (DTTD, ICAOData (Point {pntLat = 32.31666666666667, pntLon = 10.4, pntEle = Just 300.0, pntTime = Nothing}))
  , (DTTF, ICAOData (Point {pntLat = 34.416666666666664, pntLon = 8.816666666666666, pntEle = Just 313.0, pntTime = Nothing}))
  , (DTTG, ICAOData (Point {pntLat = 33.88333333333333, pntLon = 10.1, pntEle = Just 4.0, pntTime = Nothing}))
  , (DTTJ, ICAOData (Point {pntLat = 33.86666666666667, pntLon = 10.766666666666667, pntEle = Just 3.0, pntTime = Nothing}))
  , (DTTK, ICAOData (Point {pntLat = 35.666666666666664, pntLon = 10.1, pntEle = Just 60.0, pntTime = Nothing}))
  , (DTTL, ICAOData (Point {pntLat = 36.85, pntLon = 11.083333333333334, pntEle = Just 29.0, pntTime = Nothing}))
  , (DTTM, ICAOData (Point {pntLat = 35.666666666666664, pntLon = 10.75, pntEle = Just 2.0, pntTime = Nothing}))
  , (DTTN, ICAOData (Point {pntLat = 36.483333333333334, pntLon = 8.8, pntEle = Just 143.0, pntTime = Nothing}))
  , (DTTR, ICAOData (Point {pntLat = 31.683333333333334, pntLon = 9.166666666666666, pntEle = Just 258.0, pntTime = Nothing}))
  , (DTTX, ICAOData (Point {pntLat = 34.71666666666667, pntLon = 10.683333333333334, pntEle = Just 21.0, pntTime = Nothing}))
  , (DTTZ, ICAOData (Point {pntLat = 33.916666666666664, pntLon = 8.1, pntEle = Just 87.0, pntTime = Nothing}))
  , (DXAK, ICAOData (Point {pntLat = 7.583333333333333, pntLon = 1.1166666666666667, pntEle = Just 400.0, pntTime = Nothing}))
  , (DXMG, ICAOData (Point {pntLat = 10.366666666666667, pntLon = 0.4666666666666667, pntEle = Just 145.0, pntTime = Nothing}))
  , (DXNG, ICAOData (Point {pntLat = 9.766666666666667, pntLon = 1.1, pntEle = Just 342.0, pntTime = Nothing}))
  , (DXSK, ICAOData (Point {pntLat = 8.983333333333333, pntLon = 1.15, pntEle = Just 386.0, pntTime = Nothing}))
  , (DXTA, ICAOData (Point {pntLat = 6.583333333333333, pntLon = 1.5, pntEle = Just 44.0, pntTime = Nothing}))
  , (DXXX, ICAOData (Point {pntLat = 6.166666666666667, pntLon = 1.25, pntEle = Just 20.0, pntTime = Nothing}))
  , (EBAW, ICAOData (Point {pntLat = 51.2, pntLon = 4.466666666666667, pntEle = Just 12.0, pntTime = Nothing}))
  , (EBBE, ICAOData (Point {pntLat = 50.75, pntLon = 4.766666666666667, pntEle = Just 105.0, pntTime = Nothing}))
  , (EBBL, ICAOData (Point {pntLat = 51.166666666666664, pntLon = 5.466666666666667, pntEle = Just 55.0, pntTime = Nothing}))
  , (EBBR, ICAOData (Point {pntLat = 50.9, pntLon = 4.533333333333333, pntEle = Just 55.0, pntTime = Nothing}))
  , (EBBT, ICAOData (Point {pntLat = 51.333333333333336, pntLon = 4.5, pntEle = Just 22.0, pntTime = Nothing}))
  , (EBBX, ICAOData (Point {pntLat = 49.9, pntLon = 5.216666666666667, pntEle = Just 461.0, pntTime = Nothing}))
  , (EBCI, ICAOData (Point {pntLat = 50.46666666666667, pntLon = 4.45, pntEle = Just 187.0, pntTime = Nothing}))
  , (EBCV, ICAOData (Point {pntLat = 50.56666666666667, pntLon = 3.8333333333333335, pntEle = Just 60.0, pntTime = Nothing}))
  , (EBDT, ICAOData (Point {pntLat = 51.0, pntLon = 5.066666666666666, pntEle = Just 30.0, pntTime = Nothing}))
  , (EBFN, ICAOData (Point {pntLat = 51.083333333333336, pntLon = 2.65, pntEle = Just 4.0, pntTime = Nothing}))
  , (EBFS, ICAOData (Point {pntLat = 50.233333333333334, pntLon = 4.65, pntEle = Just 279.0, pntTime = Nothing}))
  , (EBGT, ICAOData (Point {pntLat = 51.18333333333333, pntLon = 3.8166666666666664, pntEle = Just 10.0, pntTime = Nothing}))
  , (EBLB, ICAOData (Point {pntLat = 50.46666666666667, pntLon = 6.183333333333334, pntEle = Just 564.0, pntTime = Nothing}))
  , (EBLG, ICAOData (Point {pntLat = 50.65, pntLon = 5.45, pntEle = Just 186.0, pntTime = Nothing}))
  , (EBMB, ICAOData (Point {pntLat = 50.9, pntLon = 4.5, pntEle = Just 55.0, pntTime = Nothing}))
  , (EBMT, ICAOData (Point {pntLat = 50.93333333333333, pntLon = 3.7333333333333334, pntEle = Just 55.0, pntTime = Nothing}))
  , (EBOS, ICAOData (Point {pntLat = 51.2, pntLon = 2.8666666666666667, pntEle = Just 4.0, pntTime = Nothing}))
  , (EBSP, ICAOData (Point {pntLat = 50.483333333333334, pntLon = 5.916666666666667, pntEle = Just 470.0, pntTime = Nothing}))
  , (EBST, ICAOData (Point {pntLat = 50.8, pntLon = 5.2, pntEle = Just 74.0, pntTime = Nothing}))
  , (EBSU, ICAOData (Point {pntLat = 50.03333333333333, pntLon = 5.4, pntEle = Just 563.0, pntTime = Nothing}))
  , (EBTN, ICAOData (Point {pntLat = 50.78333333333333, pntLon = 4.95, pntEle = Just 71.0, pntTime = Nothing}))
  , (EBWE, ICAOData (Point {pntLat = 51.416666666666664, pntLon = 5.0, pntEle = Just 60.0, pntTime = Nothing}))
  , (EBZW, ICAOData (Point {pntLat = 50.93333333333333, pntLon = 5.5, pntEle = Just 61.0, pntTime = Nothing}))
  , (EDDB, ICAOData (Point {pntLat = 52.38333333333333, pntLon = 13.516666666666667, pntEle = Just 47.0, pntTime = Nothing}))
  , (EDDC, ICAOData (Point {pntLat = 51.13333333333333, pntLon = 13.75, pntEle = Just 230.0, pntTime = Nothing}))
  , (EDDE, ICAOData (Point {pntLat = 50.983333333333334, pntLon = 10.966666666666667, pntEle = Just 315.0, pntTime = Nothing}))
  , (EDDF, ICAOData (Point {pntLat = 50.05, pntLon = 8.6, pntEle = Just 111.0, pntTime = Nothing}))
  , (EDDG, ICAOData (Point {pntLat = 52.13333333333333, pntLon = 7.7, pntEle = Just 48.0, pntTime = Nothing}))
  , (EDDH, ICAOData (Point {pntLat = 53.63333333333333, pntLon = 10.0, pntEle = Just 16.0, pntTime = Nothing}))
  , (EDDI, ICAOData (Point {pntLat = 52.46666666666667, pntLon = 13.4, pntEle = Just 50.0, pntTime = Nothing}))
  , (EDDK, ICAOData (Point {pntLat = 50.86666666666667, pntLon = 7.166666666666667, pntEle = Just 91.0, pntTime = Nothing}))
  , (EDDL, ICAOData (Point {pntLat = 51.3, pntLon = 6.766666666666667, pntEle = Just 45.0, pntTime = Nothing}))
  , (EDDM, ICAOData (Point {pntLat = 48.35, pntLon = 11.783333333333333, pntEle = Just 453.0, pntTime = Nothing}))
  , (EDDN, ICAOData (Point {pntLat = 49.5, pntLon = 11.05, pntEle = Just 318.0, pntTime = Nothing}))
  , (EDDP, ICAOData (Point {pntLat = 51.416666666666664, pntLon = 12.233333333333333, pntEle = Just 144.0, pntTime = Nothing}))
  , (EDDR, ICAOData (Point {pntLat = 49.21666666666667, pntLon = 7.116666666666666, pntEle = Just 322.0, pntTime = Nothing}))
  , (EDDS, ICAOData (Point {pntLat = 48.68333333333333, pntLon = 9.216666666666667, pntEle = Just 396.0, pntTime = Nothing}))
  , (EDDT, ICAOData (Point {pntLat = 52.56666666666667, pntLon = 13.316666666666666, pntEle = Just 37.0, pntTime = Nothing}))
  , (EDDV, ICAOData (Point {pntLat = 52.46666666666667, pntLon = 9.683333333333334, pntEle = Just 56.0, pntTime = Nothing}))
  , (EDDW, ICAOData (Point {pntLat = 53.05, pntLon = 8.8, pntEle = Just 3.0, pntTime = Nothing}))
  , (EDFH, ICAOData (Point {pntLat = 49.95, pntLon = 7.266666666666667, pntEle = Just 491.0, pntTime = Nothing}))
  , (EDFM, ICAOData (Point {pntLat = 49.46666666666667, pntLon = 8.5, pntEle = Just 100.0, pntTime = Nothing}))
  , (EDHI, ICAOData (Point {pntLat = 53.53333333333333, pntLon = 9.833333333333334, pntEle = Just 5.0, pntTime = Nothing}))
  , (EDHL, ICAOData (Point {pntLat = 53.81666666666667, pntLon = 10.7, pntEle = Just 14.0, pntTime = Nothing}))
  , (EDLP, ICAOData (Point {pntLat = 51.61666666666667, pntLon = 8.616666666666667, pntEle = Just 213.0, pntTime = Nothing}))
  , (EDLW, ICAOData (Point {pntLat = 51.516666666666666, pntLon = 7.616666666666667, pntEle = Just 127.0, pntTime = Nothing}))
  , (EDMA, ICAOData (Point {pntLat = 48.43333333333333, pntLon = 10.933333333333334, pntEle = Just 461.0, pntTime = Nothing}))
  , (EDNY, ICAOData (Point {pntLat = 47.666666666666664, pntLon = 9.516666666666667, pntEle = Just 416.0, pntTime = Nothing}))
  , (EDOP, ICAOData (Point {pntLat = 53.416666666666664, pntLon = 11.783333333333333, pntEle = Just 52.0, pntTime = Nothing}))
  , (EDQD, ICAOData (Point {pntLat = 49.983333333333334, pntLon = 11.633333333333333, pntEle = Just 488.0, pntTime = Nothing}))
  , (EDQM, ICAOData (Point {pntLat = 50.31666666666667, pntLon = 11.883333333333333, pntEle = Just 567.0, pntTime = Nothing}))
  , (EDTD, ICAOData (Point {pntLat = 47.96666666666667, pntLon = 8.516666666666667, pntEle = Just 680.0, pntTime = Nothing}))
  , (EDTZ, ICAOData (Point {pntLat = 47.68333333333333, pntLon = 9.183333333333334, pntEle = Just 443.0, pntTime = Nothing}))
  , (EDVE, ICAOData (Point {pntLat = 52.3, pntLon = 10.45, pntEle = Just 81.0, pntTime = Nothing}))
  , (EDVK, ICAOData (Point {pntLat = 51.4, pntLon = 9.383333333333333, pntEle = Just 277.0, pntTime = Nothing}))
  , (EDXW, ICAOData (Point {pntLat = 54.916666666666664, pntLon = 8.35, pntEle = Just 16.0, pntTime = Nothing}))
  , (EDZE, ICAOData (Point {pntLat = 51.4, pntLon = 6.966666666666667, pntEle = Just 152.0, pntTime = Nothing}))
  , (EEKA, ICAOData (Point {pntLat = 58.983333333333334, pntLon = 22.816666666666666, pntEle = Just 5.0, pntTime = Nothing}))
  , (EFHA, ICAOData (Point {pntLat = 61.85, pntLon = 24.8, pntEle = Just 143.0, pntTime = Nothing}))
  , (EFHF, ICAOData (Point {pntLat = 60.25, pntLon = 25.05, pntEle = Just 17.0, pntTime = Nothing}))
  , (EFHK, ICAOData (Point {pntLat = 60.31666666666667, pntLon = 24.966666666666665, pntEle = Just 51.0, pntTime = Nothing}))
  , (EFHL, ICAOData (Point {pntLat = 65.03333333333333, pntLon = 24.8, pntEle = Just 6.0, pntTime = Nothing}))
  , (EFHT, ICAOData (Point {pntLat = 62.53333333333333, pntLon = 24.216666666666665, pntEle = Just 157.0, pntTime = Nothing}))
  , (EFIV, ICAOData (Point {pntLat = 68.61666666666666, pntLon = 27.416666666666668, pntEle = Just 147.0, pntTime = Nothing}))
  , (EFJO, ICAOData (Point {pntLat = 62.666666666666664, pntLon = 29.633333333333333, pntEle = Just 119.0, pntTime = Nothing}))
  , (EFJY, ICAOData (Point {pntLat = 62.4, pntLon = 25.683333333333334, pntEle = Just 141.0, pntTime = Nothing}))
  , (EFKA, ICAOData (Point {pntLat = 63.1, pntLon = 23.033333333333335, pntEle = Just 42.0, pntTime = Nothing}))
  , (EFKE, ICAOData (Point {pntLat = 65.78333333333333, pntLon = 24.583333333333332, pntEle = Just 15.0, pntTime = Nothing}))
  , (EFKI, ICAOData (Point {pntLat = 64.28333333333333, pntLon = 27.683333333333334, pntEle = Just 143.0, pntTime = Nothing}))
  , (EFKK, ICAOData (Point {pntLat = 63.71666666666667, pntLon = 23.15, pntEle = Just 25.0, pntTime = Nothing}))
  , (EFKS, ICAOData (Point {pntLat = 65.96666666666667, pntLon = 29.183333333333334, pntEle = Just 262.0, pntTime = Nothing}))
  , (EFKU, ICAOData (Point {pntLat = 63.016666666666666, pntLon = 27.8, pntEle = Just 98.0, pntTime = Nothing}))
  , (EFLP, ICAOData (Point {pntLat = 61.03333333333333, pntLon = 28.133333333333333, pntEle = Just 106.0, pntTime = Nothing}))
  , (EFMA, ICAOData (Point {pntLat = 60.11666666666667, pntLon = 19.9, pntEle = Just 6.0, pntTime = Nothing}))
  , (EFMI, ICAOData (Point {pntLat = 61.733333333333334, pntLon = 27.3, pntEle = Just 138.0, pntTime = Nothing}))
  , (EFOU, ICAOData (Point {pntLat = 64.93333333333334, pntLon = 25.366666666666667, pntEle = Just 12.0, pntTime = Nothing}))
  , (EFPE, ICAOData (Point {pntLat = 66.8, pntLon = 24.0, pntEle = Just 84.0, pntTime = Nothing}))
  , (EFPO, ICAOData (Point {pntLat = 61.46666666666667, pntLon = 21.8, pntEle = Just 13.0, pntTime = Nothing}))
  , (EFPU, ICAOData (Point {pntLat = 65.36666666666666, pntLon = 27.016666666666666, pntEle = Just 114.0, pntTime = Nothing}))
  , (EFRO, ICAOData (Point {pntLat = 66.56666666666666, pntLon = 25.833333333333332, pntEle = Just 197.0, pntTime = Nothing}))
  , (EFSA, ICAOData (Point {pntLat = 61.95, pntLon = 28.95, pntEle = Just 95.0, pntTime = Nothing}))
  , (EFSO, ICAOData (Point {pntLat = 67.36666666666666, pntLon = 26.65, pntEle = Just 179.0, pntTime = Nothing}))
  , (EFSU, ICAOData (Point {pntLat = 64.9, pntLon = 29.016666666666666, pntEle = Just 223.0, pntTime = Nothing}))
  , (EFTP, ICAOData (Point {pntLat = 61.416666666666664, pntLon = 23.583333333333332, pntEle = Just 112.0, pntTime = Nothing}))
  , (EFTU, ICAOData (Point {pntLat = 60.516666666666666, pntLon = 22.266666666666666, pntEle = Just 49.0, pntTime = Nothing}))
  , (EFUT, ICAOData (Point {pntLat = 60.9, pntLon = 26.933333333333334, pntEle = Just 100.0, pntTime = Nothing}))
  , (EFVA, ICAOData (Point {pntLat = 63.05, pntLon = 21.766666666666666, pntEle = Just 4.0, pntTime = Nothing}))
  , (EFVI, ICAOData (Point {pntLat = 63.083333333333336, pntLon = 25.866666666666667, pntEle = Just 132.0, pntTime = Nothing}))
  , (EGAA, ICAOData (Point {pntLat = 54.65, pntLon = -6.216666666666667, pntEle = Just 81.0, pntTime = Nothing}))
  , (EGAC, ICAOData (Point {pntLat = 54.6, pntLon = -5.883333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (EGBB, ICAOData (Point {pntLat = 52.45, pntLon = -1.7333333333333334, pntEle = Just 99.0, pntTime = Nothing}))
  , (EGCC, ICAOData (Point {pntLat = 53.35, pntLon = -2.283333333333333, pntEle = Just 69.0, pntTime = Nothing}))
  , (EGDB, ICAOData (Point {pntLat = 50.35, pntLon = -4.116666666666666, pntEle = Just 50.0, pntTime = Nothing}))
  , (EGDC, ICAOData (Point {pntLat = 51.083333333333336, pntLon = -4.15, pntEle = Just 8.0, pntTime = Nothing}))
  , (EGDG, ICAOData (Point {pntLat = 50.43333333333333, pntLon = -5.0, pntEle = Just 119.0, pntTime = Nothing}))
  , (EGDL, ICAOData (Point {pntLat = 51.5, pntLon = -1.9833333333333334, pntEle = Just 145.0, pntTime = Nothing}))
  , (EGDM, ICAOData (Point {pntLat = 51.166666666666664, pntLon = -1.75, pntEle = Just 124.0, pntTime = Nothing}))
  , (EGDP, ICAOData (Point {pntLat = 50.56666666666667, pntLon = -2.45, pntEle = Just 3.0, pntTime = Nothing}))
  , (EGDR, ICAOData (Point {pntLat = 50.083333333333336, pntLon = -5.25, pntEle = Just 78.0, pntTime = Nothing}))
  , (EGDY, ICAOData (Point {pntLat = 51.0, pntLon = -2.6333333333333333, pntEle = Just 23.0, pntTime = Nothing}))
  , (EGFF, ICAOData (Point {pntLat = 51.4, pntLon = -3.35, pntEle = Just 67.0, pntTime = Nothing}))
  , (EGHE, ICAOData (Point {pntLat = 49.916666666666664, pntLon = -6.3, pntEle = Just 31.0, pntTime = Nothing}))
  , (EGHH, ICAOData (Point {pntLat = 50.78333333333333, pntLon = -1.8333333333333335, pntEle = Just 11.0, pntTime = Nothing}))
  , (EGHI, ICAOData (Point {pntLat = 50.9, pntLon = -1.4, pntEle = Just 9.0, pntTime = Nothing}))
  , (EGJB, ICAOData (Point {pntLat = 49.43333333333333, pntLon = -2.6, pntEle = Just 102.0, pntTime = Nothing}))
  , (EGJJ, ICAOData (Point {pntLat = 49.21666666666667, pntLon = -2.2, pntEle = Just 84.0, pntTime = Nothing}))
  , (EGKA, ICAOData (Point {pntLat = 50.833333333333336, pntLon = -0.2833333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (EGKK, ICAOData (Point {pntLat = 51.15, pntLon = -0.18333333333333332, pntEle = Just 62.0, pntTime = Nothing}))
  , (EGLF, ICAOData (Point {pntLat = 51.28333333333333, pntLon = -0.7666666666666667, pntEle = Just 65.0, pntTime = Nothing}))
  , (EGLL, ICAOData (Point {pntLat = 51.483333333333334, pntLon = -0.45, pntEle = Just 24.0, pntTime = Nothing}))
  , (EGNC, ICAOData (Point {pntLat = 54.93333333333333, pntLon = -2.95, pntEle = Just 26.0, pntTime = Nothing}))
  , (EGNH, ICAOData (Point {pntLat = 53.766666666666666, pntLon = -3.033333333333333, pntEle = Just 10.0, pntTime = Nothing}))
  , (EGNL, ICAOData (Point {pntLat = 54.11666666666667, pntLon = -3.25, pntEle = Just 15.0, pntTime = Nothing}))
  , (EGNR, ICAOData (Point {pntLat = 53.166666666666664, pntLon = -2.9833333333333334, pntEle = Just 10.0, pntTime = Nothing}))
  , (EGNS, ICAOData (Point {pntLat = 54.083333333333336, pntLon = -4.633333333333333, pntEle = Just 17.0, pntTime = Nothing}))
  , (EGOM, ICAOData (Point {pntLat = 55.05, pntLon = -2.55, pntEle = Just 325.0, pntTime = Nothing}))
  , (EGOP, ICAOData (Point {pntLat = 51.71666666666667, pntLon = -4.366666666666666, pntEle = Just 3.0, pntTime = Nothing}))
  , (EGOS, ICAOData (Point {pntLat = 52.8, pntLon = -2.6666666666666665, pntEle = Just 76.0, pntTime = Nothing}))
  , (EGOV, ICAOData (Point {pntLat = 53.25, pntLon = -4.533333333333333, pntEle = Just 1.0, pntTime = Nothing}))
  , (EGOY, ICAOData (Point {pntLat = 54.85, pntLon = -4.95, pntEle = Just 11.0, pntTime = Nothing}))
  , (EGPA, ICAOData (Point {pntLat = 58.95, pntLon = -2.9, pntEle = Just 21.0, pntTime = Nothing}))
  , (EGPB, ICAOData (Point {pntLat = 59.88333333333333, pntLon = -1.3, pntEle = Just 5.0, pntTime = Nothing}))
  , (EGPC, ICAOData (Point {pntLat = 58.45, pntLon = -3.0833333333333335, pntEle = Just 39.0, pntTime = Nothing}))
  , (EGPD, ICAOData (Point {pntLat = 57.2, pntLon = -2.216666666666667, pntEle = Just 65.0, pntTime = Nothing}))
  , (EGPE, ICAOData (Point {pntLat = 57.53333333333333, pntLon = -4.05, pntEle = Just 9.0, pntTime = Nothing}))
  , (EGPF, ICAOData (Point {pntLat = 55.86666666666667, pntLon = -4.433333333333334, pntEle = Just 8.0, pntTime = Nothing}))
  , (EGPH, ICAOData (Point {pntLat = 55.95, pntLon = -3.35, pntEle = Just 41.0, pntTime = Nothing}))
  , (EGPK, ICAOData (Point {pntLat = 55.5, pntLon = -4.583333333333333, pntEle = Just 20.0, pntTime = Nothing}))
  , (EGPL, ICAOData (Point {pntLat = 57.46666666666667, pntLon = -7.366666666666666, pntEle = Just 6.0, pntTime = Nothing}))
  , (EGPO, ICAOData (Point {pntLat = 58.21666666666667, pntLon = -6.316666666666666, pntEle = Just 9.0, pntTime = Nothing}))
  , (EGPU, ICAOData (Point {pntLat = 56.5, pntLon = -6.883333333333333, pntEle = Just 12.0, pntTime = Nothing}))
  , (EGQA, ICAOData (Point {pntLat = 57.81666666666667, pntLon = -3.966666666666667, pntEle = Just 4.0, pntTime = Nothing}))
  , (EGQJ, ICAOData (Point {pntLat = 55.43333333333333, pntLon = -5.7, pntEle = Just 13.0, pntTime = Nothing}))
  , (EGQK, ICAOData (Point {pntLat = 57.65, pntLon = -3.5666666666666664, pntEle = Just 7.0, pntTime = Nothing}))
  , (EGQL, ICAOData (Point {pntLat = 56.38333333333333, pntLon = -2.8666666666666667, pntEle = Just 12.0, pntTime = Nothing}))
  , (EGQM, ICAOData (Point {pntLat = 55.416666666666664, pntLon = -1.6, pntEle = Just 23.0, pntTime = Nothing}))
  , (EGQS, ICAOData (Point {pntLat = 57.71666666666667, pntLon = -3.3166666666666664, pntEle = Just 13.0, pntTime = Nothing}))
  , (EGRB, ICAOData (Point {pntLat = 51.516666666666666, pntLon = -0.11666666666666667, pntEle = Just 5.0, pntTime = Nothing}))
  , (EGRR, ICAOData (Point {pntLat = 51.38333333333333, pntLon = -0.7833333333333333, pntEle = Just 74.0, pntTime = Nothing}))
  , (EGSH, ICAOData (Point {pntLat = 52.63333333333333, pntLon = 1.3, pntEle = Just 14.0, pntTime = Nothing}))
  , (EGSS, ICAOData (Point {pntLat = 51.88333333333333, pntLon = 0.23333333333333334, pntEle = Just 106.0, pntTime = Nothing}))
  , (EGTE, ICAOData (Point {pntLat = 50.733333333333334, pntLon = -3.4166666666666665, pntEle = Just 30.0, pntTime = Nothing}))
  , (EGUB, ICAOData (Point {pntLat = 51.61666666666667, pntLon = -1.0833333333333333, pntEle = Just 63.0, pntTime = Nothing}))
  , (EGUC, ICAOData (Point {pntLat = 52.13333333333333, pntLon = -4.566666666666666, pntEle = Just 133.0, pntTime = Nothing}))
  , (EGUF, ICAOData (Point {pntLat = 51.28333333333333, pntLon = -0.7666666666666667, pntEle = Just 72.0, pntTime = Nothing}))
  , (EGUL, ICAOData (Point {pntLat = 52.416666666666664, pntLon = 0.5666666666666667, pntEle = Just 10.0, pntTime = Nothing}))
  , (EGUM, ICAOData (Point {pntLat = 51.35, pntLon = 1.35, pntEle = Just 55.0, pntTime = Nothing}))
  , (EGUN, ICAOData (Point {pntLat = 52.36666666666667, pntLon = 0.48333333333333334, pntEle = Just 10.0, pntTime = Nothing}))
  , (EGUW, ICAOData (Point {pntLat = 52.11666666666667, pntLon = 0.9666666666666667, pntEle = Just 87.0, pntTime = Nothing}))
  , (EGUY, ICAOData (Point {pntLat = 52.35, pntLon = -0.11666666666666667, pntEle = Just 41.0, pntTime = Nothing}))
  , (EGVA, ICAOData (Point {pntLat = 51.68333333333333, pntLon = -1.7833333333333332, pntEle = Just 87.0, pntTime = Nothing}))
  , (EGVN, ICAOData (Point {pntLat = 51.75, pntLon = -1.5833333333333335, pntEle = Just 88.0, pntTime = Nothing}))
  , (EGVO, ICAOData (Point {pntLat = 51.233333333333334, pntLon = -0.95, pntEle = Just 123.0, pntTime = Nothing}))
  , (EGVP, ICAOData (Point {pntLat = 51.15, pntLon = -1.5666666666666667, pntEle = Just 91.0, pntTime = Nothing}))
  , (EGWU, ICAOData (Point {pntLat = 51.55, pntLon = -0.4166666666666667, pntEle = Just 38.0, pntTime = Nothing}))
  , (EGWZ, ICAOData (Point {pntLat = 52.36666666666667, pntLon = -0.21666666666666667, pntEle = Just 49.0, pntTime = Nothing}))
  , (EGXC, ICAOData (Point {pntLat = 53.083333333333336, pntLon = -0.16666666666666666, pntEle = Just 7.0, pntTime = Nothing}))
  , (EGXD, ICAOData (Point {pntLat = 54.13333333333333, pntLon = -1.4166666666666667, pntEle = Just 36.0, pntTime = Nothing}))
  , (EGXE, ICAOData (Point {pntLat = 54.3, pntLon = -1.5333333333333332, pntEle = Just 40.0, pntTime = Nothing}))
  , (EGXG, ICAOData (Point {pntLat = 53.833333333333336, pntLon = -1.2, pntEle = Just 9.0, pntTime = Nothing}))
  , (EGXH, ICAOData (Point {pntLat = 52.333333333333336, pntLon = 0.7666666666666667, pntEle = Just 54.0, pntTime = Nothing}))
  , (EGXJ, ICAOData (Point {pntLat = 52.733333333333334, pntLon = -0.65, pntEle = Just 138.0, pntTime = Nothing}))
  , (EGXT, ICAOData (Point {pntLat = 52.61666666666667, pntLon = -0.4666666666666667, pntEle = Just 84.0, pntTime = Nothing}))
  , (EGXU, ICAOData (Point {pntLat = 54.05, pntLon = -1.25, pntEle = Just 16.0, pntTime = Nothing}))
  , (EGXV, ICAOData (Point {pntLat = 53.86666666666667, pntLon = -0.43333333333333335, pntEle = Just 6.0, pntTime = Nothing}))
  , (EGXW, ICAOData (Point {pntLat = 53.166666666666664, pntLon = -0.5166666666666667, pntEle = Just 68.0, pntTime = Nothing}))
  , (EGYC, ICAOData (Point {pntLat = 52.43333333333333, pntLon = 1.35, pntEle = Just 20.0, pntTime = Nothing}))
  , (EGYD, ICAOData (Point {pntLat = 53.03333333333333, pntLon = -0.5, pntEle = Just 67.0, pntTime = Nothing}))
  , (EGYH, ICAOData (Point {pntLat = 52.86666666666667, pntLon = 0.15, pntEle = Just 3.0, pntTime = Nothing}))
  , (EGYM, ICAOData (Point {pntLat = 52.65, pntLon = 0.5666666666666667, pntEle = Just 23.0, pntTime = Nothing}))
  , (EGYP, ICAOData (Point {pntLat = -51.81666666666667, pntLon = -58.45, pntEle = Just 74.0, pntTime = Nothing}))
  , (EGYW, ICAOData (Point {pntLat = 53.083333333333336, pntLon = 0.26666666666666666, pntEle = Just 3.0, pntTime = Nothing}))
  , (EHAM, ICAOData (Point {pntLat = 52.3, pntLon = 4.766666666666667, pntEle = Just (-4.0), pntTime = Nothing}))
  , (EHBK, ICAOData (Point {pntLat = 50.916666666666664, pntLon = 5.783333333333333, pntEle = Just 114.0, pntTime = Nothing}))
  , (EHDB, ICAOData (Point {pntLat = 52.1, pntLon = 5.183333333333334, pntEle = Just 2.0, pntTime = Nothing}))
  , (EHDL, ICAOData (Point {pntLat = 52.06666666666667, pntLon = 5.883333333333333, pntEle = Just 48.0, pntTime = Nothing}))
  , (EHEH, ICAOData (Point {pntLat = 51.45, pntLon = 5.416666666666667, pntEle = Just 23.0, pntTime = Nothing}))
  , (EHGG, ICAOData (Point {pntLat = 53.13333333333333, pntLon = 6.583333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (EHGR, ICAOData (Point {pntLat = 51.56666666666667, pntLon = 4.933333333333334, pntEle = Just 15.0, pntTime = Nothing}))
  , (EHKD, ICAOData (Point {pntLat = 52.916666666666664, pntLon = 4.783333333333333, pntEle = Just 0.0, pntTime = Nothing}))
  , (EHLE, ICAOData (Point {pntLat = 52.45, pntLon = 5.516666666666667, pntEle = Just 0.0, pntTime = Nothing}))
  , (EHLW, ICAOData (Point {pntLat = 53.21666666666667, pntLon = 5.766666666666667, pntEle = Just 1.0, pntTime = Nothing}))
  , (EHRD, ICAOData (Point {pntLat = 51.95, pntLon = 4.45, pntEle = Just (-5.0), pntTime = Nothing}))
  , (EHSB, ICAOData (Point {pntLat = 52.13333333333333, pntLon = 5.283333333333333, pntEle = Just 20.0, pntTime = Nothing}))
  , (EHTW, ICAOData (Point {pntLat = 52.266666666666666, pntLon = 6.9, pntEle = Just 35.0, pntTime = Nothing}))
  , (EHVB, ICAOData (Point {pntLat = 52.18333333333333, pntLon = 4.416666666666667, pntEle = Just 0.0, pntTime = Nothing}))
  , (EHVK, ICAOData (Point {pntLat = 51.65, pntLon = 5.7, pntEle = Just 22.0, pntTime = Nothing}))
  , (EHVL, ICAOData (Point {pntLat = 53.25, pntLon = 4.916666666666667, pntEle = Just 11.0, pntTime = Nothing}))
  , (EHWO, ICAOData (Point {pntLat = 51.45, pntLon = 4.333333333333333, pntEle = Just 20.0, pntTime = Nothing}))
  , (EICK, ICAOData (Point {pntLat = 51.85, pntLon = -8.483333333333333, pntEle = Just 153.0, pntTime = Nothing}))
  , (EIDW, ICAOData (Point {pntLat = 53.43333333333333, pntLon = -6.25, pntEle = Just 68.0, pntTime = Nothing}))
  , (EIME, ICAOData (Point {pntLat = 53.3, pntLon = -6.433333333333334, pntEle = Just 97.0, pntTime = Nothing}))
  , (EINN, ICAOData (Point {pntLat = 52.7, pntLon = -8.916666666666666, pntEle = Just 14.0, pntTime = Nothing}))
  , (EKAH, ICAOData (Point {pntLat = 56.3, pntLon = 10.616666666666667, pntEle = Just 23.0, pntTime = Nothing}))
  , (EKAT, ICAOData (Point {pntLat = 56.7, pntLon = 11.55, pntEle = Just 8.0, pntTime = Nothing}))
  , (EKAV, ICAOData (Point {pntLat = 55.083333333333336, pntLon = 11.783333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (EKBI, ICAOData (Point {pntLat = 55.733333333333334, pntLon = 9.166666666666666, pntEle = Just 75.0, pntTime = Nothing}))
  , (EKCH, ICAOData (Point {pntLat = 55.61666666666667, pntLon = 12.65, pntEle = Just 5.0, pntTime = Nothing}))
  , (EKEB, ICAOData (Point {pntLat = 55.53333333333333, pntLon = 8.566666666666666, pntEle = Just 30.0, pntTime = Nothing}))
  , (EKHO, ICAOData (Point {pntLat = 56.4, pntLon = 8.45, pntEle = Just 15.0, pntTime = Nothing}))
  , (EKHS, ICAOData (Point {pntLat = 56.71666666666667, pntLon = 10.116666666666667, pntEle = Just 2.0, pntTime = Nothing}))
  , (EKKA, ICAOData (Point {pntLat = 56.3, pntLon = 9.116666666666667, pntEle = Just 52.0, pntTime = Nothing}))
  , (EKMB, ICAOData (Point {pntLat = 54.7, pntLon = 11.45, pntEle = Just 5.0, pntTime = Nothing}))
  , (EKOD, ICAOData (Point {pntLat = 55.483333333333334, pntLon = 10.333333333333334, pntEle = Just 17.0, pntTime = Nothing}))
  , (EKRK, ICAOData (Point {pntLat = 55.583333333333336, pntLon = 12.133333333333333, pntEle = Just 44.0, pntTime = Nothing}))
  , (EKRN, ICAOData (Point {pntLat = 55.06666666666667, pntLon = 14.75, pntEle = Just 16.0, pntTime = Nothing}))
  , (EKSB, ICAOData (Point {pntLat = 54.96666666666667, pntLon = 9.783333333333333, pntEle = Just 7.0, pntTime = Nothing}))
  , (EKSN, ICAOData (Point {pntLat = 57.5, pntLon = 10.216666666666667, pntEle = Just 28.0, pntTime = Nothing}))
  , (EKSP, ICAOData (Point {pntLat = 55.233333333333334, pntLon = 9.266666666666667, pntEle = Just 43.0, pntTime = Nothing}))
  , (EKST, ICAOData (Point {pntLat = 55.016666666666666, pntLon = 10.566666666666666, pntEle = Just 6.0, pntTime = Nothing}))
  , (EKSV, ICAOData (Point {pntLat = 56.55, pntLon = 9.166666666666666, pntEle = Just 23.0, pntTime = Nothing}))
  , (EKTS, ICAOData (Point {pntLat = 57.06666666666667, pntLon = 8.716666666666667, pntEle = Just 7.0, pntTime = Nothing}))
  , (EKVA, ICAOData (Point {pntLat = 55.7, pntLon = 9.2, pntEle = Just 73.0, pntTime = Nothing}))
  , (EKVD, ICAOData (Point {pntLat = 55.43333333333333, pntLon = 9.333333333333334, pntEle = Just 44.0, pntTime = Nothing}))
  , (EKVG, ICAOData (Point {pntLat = 62.06666666666667, pntLon = -7.283333333333333, pntEle = Just 85.0, pntTime = Nothing}))
  , (EKVJ, ICAOData (Point {pntLat = 56.0, pntLon = 8.35, pntEle = Just 5.0, pntTime = Nothing}))
  , (EKVL, ICAOData (Point {pntLat = 55.766666666666666, pntLon = 12.333333333333334, pntEle = Just 18.0, pntTime = Nothing}))
  , (EKYT, ICAOData (Point {pntLat = 57.1, pntLon = 9.85, pntEle = Just 3.0, pntTime = Nothing}))
  , (ELLX, ICAOData (Point {pntLat = 49.61666666666667, pntLon = 6.216666666666667, pntEle = Just 376.0, pntTime = Nothing}))
  , (ENAL, ICAOData (Point {pntLat = 62.56666666666667, pntLon = 6.116666666666666, pntEle = Just 2.0, pntTime = Nothing}))
  , (ENAN, ICAOData (Point {pntLat = 69.3, pntLon = 16.15, pntEle = Just 1.0, pntTime = Nothing}))
  , (ENAS, ICAOData (Point {pntLat = 78.91666666666667, pntLon = 11.933333333333334, pntEle = Just 8.0, pntTime = Nothing}))
  , (ENAT, ICAOData (Point {pntLat = 69.98333333333333, pntLon = 23.366666666666667, pntEle = Just 0.0, pntTime = Nothing}))
  , (ENBJ, ICAOData (Point {pntLat = 74.51666666666667, pntLon = 19.016666666666666, pntEle = Just 16.0, pntTime = Nothing}))
  , (ENBM, ICAOData (Point {pntLat = 60.65, pntLon = 6.5, pntEle = Just 125.0, pntTime = Nothing}))
  , (ENBN, ICAOData (Point {pntLat = 65.46666666666667, pntLon = 12.216666666666667, pntEle = Just 0.0, pntTime = Nothing}))
  , (ENBO, ICAOData (Point {pntLat = 67.26666666666667, pntLon = 14.366666666666667, pntEle = Just 1.0, pntTime = Nothing}))
  , (ENBR, ICAOData (Point {pntLat = 60.28333333333333, pntLon = 5.233333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (ENBV, ICAOData (Point {pntLat = 70.86666666666666, pntLon = 29.033333333333335, pntEle = Just 1.0, pntTime = Nothing}))
  , (ENCN, ICAOData (Point {pntLat = 58.2, pntLon = 8.083333333333334, pntEle = Just 1.0, pntTime = Nothing}))
  , (ENDU, ICAOData (Point {pntLat = 69.06666666666666, pntLon = 18.533333333333335, pntEle = Just 7.0, pntTime = Nothing}))
  , (ENEV, ICAOData (Point {pntLat = 68.51666666666667, pntLon = 16.683333333333334, pntEle = Just 26.0, pntTime = Nothing}))
  , (ENFB, ICAOData (Point {pntLat = 59.9, pntLon = 10.633333333333333, pntEle = Just 1.0, pntTime = Nothing}))
  , (ENFG, ICAOData (Point {pntLat = 61.0, pntLon = 9.3, pntEle = Just 822.0, pntTime = Nothing}))
  , (ENFL, ICAOData (Point {pntLat = 61.583333333333336, pntLon = 5.033333333333333, pntEle = Just 0.0, pntTime = Nothing}))
  , (ENGM, ICAOData (Point {pntLat = 60.2, pntLon = 11.083333333333334, pntEle = Just 20.0, pntTime = Nothing}))
  , (ENHD, ICAOData (Point {pntLat = 59.35, pntLon = 5.216666666666667, pntEle = Just 2.0, pntTime = Nothing}))
  , (ENHF, ICAOData (Point {pntLat = 70.66666666666667, pntLon = 23.666666666666668, pntEle = Just 8.0, pntTime = Nothing}))
  , (ENHO, ICAOData (Point {pntLat = 76.5, pntLon = 25.066666666666666, pntEle = Just 6.0, pntTime = Nothing}))
  , (ENHV, ICAOData (Point {pntLat = 71.01666666666667, pntLon = 25.983333333333334, pntEle = Just 1.0, pntTime = Nothing}))
  , (ENJA, ICAOData (Point {pntLat = 70.93333333333334, pntLon = -8.666666666666666, pntEle = Just 10.0, pntTime = Nothing}))
  , (ENKA, ICAOData (Point {pntLat = 69.0, pntLon = 23.033333333333335, pntEle = Just 305.0, pntTime = Nothing}))
  , (ENKB, ICAOData (Point {pntLat = 63.11666666666667, pntLon = 7.833333333333333, pntEle = Just 6.0, pntTime = Nothing}))
  , (ENKR, ICAOData (Point {pntLat = 69.73333333333333, pntLon = 29.9, pntEle = Just 9.0, pntTime = Nothing}))
  , (ENLI, ICAOData (Point {pntLat = 58.1, pntLon = 6.633333333333333, pntEle = Just 1.0, pntTime = Nothing}))
  , (ENLK, ICAOData (Point {pntLat = 68.15, pntLon = 13.616666666666667, pntEle = Just 2.0, pntTime = Nothing}))
  , (ENMH, ICAOData (Point {pntLat = 71.03333333333333, pntLon = 27.833333333333332, pntEle = Just 1.0, pntTime = Nothing}))
  , (ENML, ICAOData (Point {pntLat = 62.75, pntLon = 7.266666666666667, pntEle = Just 0.0, pntTime = Nothing}))
  , (ENMS, ICAOData (Point {pntLat = 65.8, pntLon = 13.133333333333333, pntEle = Just 70.0, pntTime = Nothing}))
  , (ENNA, ICAOData (Point {pntLat = 70.06666666666666, pntLon = 24.983333333333334, pntEle = Just 0.0, pntTime = Nothing}))
  , (ENNK, ICAOData (Point {pntLat = 68.46666666666667, pntLon = 17.5, pntEle = Just 17.0, pntTime = Nothing}))
  , (ENNM, ICAOData (Point {pntLat = 64.46666666666667, pntLon = 11.583333333333334, pntEle = Just 0.0, pntTime = Nothing}))
  , (ENOL, ICAOData (Point {pntLat = 63.7, pntLon = 9.6, pntEle = Just 0.0, pntTime = Nothing}))
  , (ENOV, ICAOData (Point {pntLat = 62.18333333333333, pntLon = 6.066666666666666, pntEle = Just 7.0, pntTime = Nothing}))
  , (ENRA, ICAOData (Point {pntLat = 66.36666666666666, pntLon = 14.3, pntEle = Just 7.0, pntTime = Nothing}))
  , (ENRM, ICAOData (Point {pntLat = 64.83333333333333, pntLon = 11.15, pntEle = Just 4.0, pntTime = Nothing}))
  , (ENRO, ICAOData (Point {pntLat = 62.583333333333336, pntLon = 11.35, pntEle = Just 62.0, pntTime = Nothing}))
  , (ENRS, ICAOData (Point {pntLat = 67.51666666666667, pntLon = 12.1, pntEle = Just 4.0, pntTime = Nothing}))
  , (ENRY, ICAOData (Point {pntLat = 59.38333333333333, pntLon = 10.783333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (ENSB, ICAOData (Point {pntLat = 78.25, pntLon = 15.466666666666667, pntEle = Just 2.0, pntTime = Nothing}))
  , (ENSD, ICAOData (Point {pntLat = 61.833333333333336, pntLon = 6.116666666666666, pntEle = Just 6.0, pntTime = Nothing}))
  , (ENSG, ICAOData (Point {pntLat = 61.15, pntLon = 7.133333333333334, pntEle = Just 49.0, pntTime = Nothing}))
  , (ENSH, ICAOData (Point {pntLat = 68.25, pntLon = 14.666666666666666, pntEle = Just 0.0, pntTime = Nothing}))
  , (ENSK, ICAOData (Point {pntLat = 68.58333333333333, pntLon = 15.016666666666667, pntEle = Just 0.0, pntTime = Nothing}))
  , (ENSN, ICAOData (Point {pntLat = 59.18333333333333, pntLon = 9.566666666666666, pntEle = Just 1.0, pntTime = Nothing}))
  , (ENSR, ICAOData (Point {pntLat = 69.78333333333333, pntLon = 20.966666666666665, pntEle = Just 0.0, pntTime = Nothing}))
  , (ENSS, ICAOData (Point {pntLat = 70.35, pntLon = 31.05, pntEle = Just 12.0, pntTime = Nothing}))
  , (ENST, ICAOData (Point {pntLat = 65.96666666666667, pntLon = 12.466666666666667, pntEle = Just 1.0, pntTime = Nothing}))
  , (ENTC, ICAOData (Point {pntLat = 69.68333333333334, pntLon = 18.916666666666668, pntEle = Just 1.0, pntTime = Nothing}))
  , (ENTO, ICAOData (Point {pntLat = 59.2, pntLon = 10.266666666666667, pntEle = Just 8.0, pntTime = Nothing}))
  , (ENVA, ICAOData (Point {pntLat = 63.46666666666667, pntLon = 10.933333333333334, pntEle = Just 1.0, pntTime = Nothing}))
  , (ENVD, ICAOData (Point {pntLat = 70.06666666666666, pntLon = 29.85, pntEle = Just 3.0, pntTime = Nothing}))
  , (ENZV, ICAOData (Point {pntLat = 58.88333333333333, pntLon = 5.633333333333333, pntEle = Just 0.0, pntTime = Nothing}))
  , (EPGD, ICAOData (Point {pntLat = 54.38333333333333, pntLon = 18.466666666666665, pntEle = Just 135.0, pntTime = Nothing}))
  , (EPKK, ICAOData (Point {pntLat = 50.083333333333336, pntLon = 19.8, pntEle = Just 237.0, pntTime = Nothing}))
  , (EPKO, ICAOData (Point {pntLat = 54.2, pntLon = 16.15, pntEle = Just 32.0, pntTime = Nothing}))
  , (EPKT, ICAOData (Point {pntLat = 50.233333333333334, pntLon = 19.033333333333335, pntEle = Just 284.0, pntTime = Nothing}))
  , (EPPO, ICAOData (Point {pntLat = 52.416666666666664, pntLon = 16.833333333333332, pntEle = Just 86.0, pntTime = Nothing}))
  , (EPRZ, ICAOData (Point {pntLat = 50.1, pntLon = 22.05, pntEle = Just 200.0, pntTime = Nothing}))
  , (EPSC, ICAOData (Point {pntLat = 53.4, pntLon = 14.616666666666667, pntEle = Just 1.0, pntTime = Nothing}))
  , (EPWA, ICAOData (Point {pntLat = 52.166666666666664, pntLon = 20.966666666666665, pntEle = Just 106.0, pntTime = Nothing}))
  , (EPWR, ICAOData (Point {pntLat = 51.1, pntLon = 16.883333333333333, pntEle = Just 120.0, pntTime = Nothing}))
  , (EPZG, ICAOData (Point {pntLat = 51.93333333333333, pntLon = 15.533333333333333, pntEle = Just 192.0, pntTime = Nothing}))
  , (ESCF, ICAOData (Point {pntLat = 58.4, pntLon = 15.516666666666667, pntEle = Just 93.0, pntTime = Nothing}))
  , (ESCL, ICAOData (Point {pntLat = 61.266666666666666, pntLon = 17.1, pntEle = Just 26.0, pntTime = Nothing}))
  , (ESCM, ICAOData (Point {pntLat = 59.88333333333333, pntLon = 17.6, pntEle = Just 21.0, pntTime = Nothing}))
  , (ESDA, ICAOData (Point {pntLat = 56.083333333333336, pntLon = 13.2, pntEle = Just 52.0, pntTime = Nothing}))
  , (ESDB, ICAOData (Point {pntLat = 56.3, pntLon = 12.85, pntEle = Just 20.0, pntTime = Nothing}))
  , (ESDF, ICAOData (Point {pntLat = 56.266666666666666, pntLon = 15.283333333333333, pntEle = Just 58.0, pntTime = Nothing}))
  , (ESGG, ICAOData (Point {pntLat = 57.666666666666664, pntLon = 12.3, pntEle = Just 169.0, pntTime = Nothing}))
  , (ESGJ, ICAOData (Point {pntLat = 57.766666666666666, pntLon = 14.083333333333334, pntEle = Just 226.0, pntTime = Nothing}))
  , (ESGP, ICAOData (Point {pntLat = 57.78333333333333, pntLon = 11.883333333333333, pntEle = Just 20.0, pntTime = Nothing}))
  , (ESGR, ICAOData (Point {pntLat = 58.45, pntLon = 13.966666666666667, pntEle = Just 98.0, pntTime = Nothing}))
  , (ESIB, ICAOData (Point {pntLat = 58.43333333333333, pntLon = 12.7, pntEle = Just 54.0, pntTime = Nothing}))
  , (ESMK, ICAOData (Point {pntLat = 55.916666666666664, pntLon = 14.083333333333334, pntEle = Just 23.0, pntTime = Nothing}))
  , (ESMQ, ICAOData (Point {pntLat = 56.733333333333334, pntLon = 16.3, pntEle = Just 16.0, pntTime = Nothing}))
  , (ESMS, ICAOData (Point {pntLat = 55.55, pntLon = 13.366666666666667, pntEle = Just 106.0, pntTime = Nothing}))
  , (ESMT, ICAOData (Point {pntLat = 56.68333333333333, pntLon = 12.833333333333334, pntEle = Just 30.0, pntTime = Nothing}))
  , (ESMV, ICAOData (Point {pntLat = 57.3, pntLon = 14.133333333333333, pntEle = Just 169.0, pntTime = Nothing}))
  , (ESMX, ICAOData (Point {pntLat = 56.85, pntLon = 14.833333333333334, pntEle = Just 199.0, pntTime = Nothing}))
  , (ESNG, ICAOData (Point {pntLat = 67.15, pntLon = 20.65, pntEle = Just 359.0, pntTime = Nothing}))
  , (ESNJ, ICAOData (Point {pntLat = 66.63333333333334, pntLon = 19.65, pntEle = Just 263.0, pntTime = Nothing}))
  , (ESNK, ICAOData (Point {pntLat = 63.05, pntLon = 17.766666666666666, pntEle = Just 10.0, pntTime = Nothing}))
  , (ESNN, ICAOData (Point {pntLat = 62.53333333333333, pntLon = 17.45, pntEle = Just 4.0, pntTime = Nothing}))
  , (ESNO, ICAOData (Point {pntLat = 63.4, pntLon = 18.966666666666665, pntEle = Just 103.0, pntTime = Nothing}))
  , (ESNQ, ICAOData (Point {pntLat = 67.81666666666666, pntLon = 20.333333333333332, pntEle = Just 452.0, pntTime = Nothing}))
  , (ESNS, ICAOData (Point {pntLat = 64.63333333333334, pntLon = 21.083333333333332, pntEle = Just 49.0, pntTime = Nothing}))
  , (ESNU, ICAOData (Point {pntLat = 63.8, pntLon = 20.283333333333335, pntEle = Just 7.0, pntTime = Nothing}))
  , (ESOW, ICAOData (Point {pntLat = 59.583333333333336, pntLon = 16.633333333333333, pntEle = Just 6.0, pntTime = Nothing}))
  , (ESPA, ICAOData (Point {pntLat = 65.55, pntLon = 22.133333333333333, pntEle = Just 17.0, pntTime = Nothing}))
  , (ESPC, ICAOData (Point {pntLat = 63.18333333333333, pntLon = 14.5, pntEle = Just 376.0, pntTime = Nothing}))
  , (ESPD, ICAOData (Point {pntLat = 64.96666666666667, pntLon = 17.7, pntEle = Just 280.0, pntTime = Nothing}))
  , (ESPE, ICAOData (Point {pntLat = 65.86666666666666, pntLon = 20.133333333333333, pntEle = Just 180.0, pntTime = Nothing}))
  , (ESSA, ICAOData (Point {pntLat = 59.65, pntLon = 17.95, pntEle = Just 38.0, pntTime = Nothing}))
  , (ESSB, ICAOData (Point {pntLat = 59.35, pntLon = 17.95, pntEle = Just 14.0, pntTime = Nothing}))
  , (ESSD, ICAOData (Point {pntLat = 60.43333333333333, pntLon = 15.516666666666667, pntEle = Just 145.0, pntTime = Nothing}))
  , (ESSF, ICAOData (Point {pntLat = 57.516666666666666, pntLon = 15.833333333333334, pntEle = Just 112.0, pntTime = Nothing}))
  , (ESSK, ICAOData (Point {pntLat = 60.6, pntLon = 16.95, pntEle = Just 79.0, pntTime = Nothing}))
  , (ESSP, ICAOData (Point {pntLat = 58.583333333333336, pntLon = 16.15, pntEle = Just 34.0, pntTime = Nothing}))
  , (ESSQ, ICAOData (Point {pntLat = 59.36666666666667, pntLon = 13.466666666666667, pntEle = Just 46.0, pntTime = Nothing}))
  , (ESSV, ICAOData (Point {pntLat = 57.666666666666664, pntLon = 18.35, pntEle = Just 51.0, pntTime = Nothing}))
  , (ETAD, ICAOData (Point {pntLat = 49.983333333333334, pntLon = 6.7, pntEle = Just 365.0, pntTime = Nothing}))
  , (ETAR, ICAOData (Point {pntLat = 49.43333333333333, pntLon = 7.6, pntEle = Just 238.0, pntTime = Nothing}))
  , (ETAS, ICAOData (Point {pntLat = 49.5, pntLon = 7.866666666666667, pntEle = Just 321.0, pntTime = Nothing}))
  , (ETBA, ICAOData (Point {pntLat = 50.81666666666667, pntLon = 6.183333333333334, pntEle = Just 191.0, pntTime = Nothing}))
  , (ETEB, ICAOData (Point {pntLat = 49.31666666666667, pntLon = 10.633333333333333, pntEle = Just 467.0, pntTime = Nothing}))
  , (ETEH, ICAOData (Point {pntLat = 49.85, pntLon = 7.883333333333333, pntEle = Just 103.0, pntTime = Nothing}))
  , (ETEU, ICAOData (Point {pntLat = 49.65, pntLon = 9.966666666666667, pntEle = Just 298.0, pntTime = Nothing}))
  , (ETGB, ICAOData (Point {pntLat = 52.81666666666667, pntLon = 9.933333333333334, pntEle = Just 70.0, pntTime = Nothing}))
  , (ETGG, ICAOData (Point {pntLat = 54.833333333333336, pntLon = 9.5, pntEle = Just 27.0, pntTime = Nothing}))
  , (ETGI, ICAOData (Point {pntLat = 49.7, pntLon = 7.333333333333333, pntEle = Just 376.0, pntTime = Nothing}))
  , (ETGK, ICAOData (Point {pntLat = 49.43333333333333, pntLon = 11.9, pntEle = Just 419.0, pntTime = Nothing}))
  , (ETGW, ICAOData (Point {pntLat = 53.2, pntLon = 12.516666666666667, pntEle = Just 72.0, pntTime = Nothing}))
  , (ETGY, ICAOData (Point {pntLat = 51.733333333333334, pntLon = 6.266666666666667, pntEle = Just 31.0, pntTime = Nothing}))
  , (ETGZ, ICAOData (Point {pntLat = 48.18333333333333, pntLon = 9.0, pntEle = Just 920.0, pntTime = Nothing}))
  , (ETHA, ICAOData (Point {pntLat = 47.833333333333336, pntLon = 10.866666666666667, pntEle = Just 739.0, pntTime = Nothing}))
  , (ETHB, ICAOData (Point {pntLat = 52.28333333333333, pntLon = 9.083333333333334, pntEle = Just 70.0, pntTime = Nothing}))
  , (ETHC, ICAOData (Point {pntLat = 52.6, pntLon = 10.016666666666667, pntEle = Just 39.0, pntTime = Nothing}))
  , (ETHE, ICAOData (Point {pntLat = 52.3, pntLon = 7.383333333333334, pntEle = Just 40.0, pntTime = Nothing}))
  , (ETHF, ICAOData (Point {pntLat = 51.11666666666667, pntLon = 9.283333333333333, pntEle = Just 172.0, pntTime = Nothing}))
  , (ETHI, ICAOData (Point {pntLat = 54.0, pntLon = 9.583333333333334, pntEle = Just 25.0, pntTime = Nothing}))
  , (ETHL, ICAOData (Point {pntLat = 48.21666666666667, pntLon = 9.916666666666666, pntEle = Just 538.0, pntTime = Nothing}))
  , (ETHM, ICAOData (Point {pntLat = 50.36666666666667, pntLon = 7.316666666666666, pntEle = Just 182.0, pntTime = Nothing}))
  , (ETHN, ICAOData (Point {pntLat = 49.4, pntLon = 9.966666666666667, pntEle = Just 468.0, pntTime = Nothing}))
  , (ETHR, ICAOData (Point {pntLat = 49.21666666666667, pntLon = 11.1, pntEle = Just 388.0, pntTime = Nothing}))
  , (ETHS, ICAOData (Point {pntLat = 52.916666666666664, pntLon = 10.183333333333334, pntEle = Just 75.0, pntTime = Nothing}))
  , (ETHT, ICAOData (Point {pntLat = 51.766666666666666, pntLon = 14.3, pntEle = Just 67.0, pntTime = Nothing}))
  , (ETIC, ICAOData (Point {pntLat = 49.7, pntLon = 11.95, pntEle = Just 415.0, pntTime = Nothing}))
  , (ETID, ICAOData (Point {pntLat = 50.166666666666664, pntLon = 8.966666666666667, pntEle = Just 112.0, pntTime = Nothing}))
  , (ETIE, ICAOData (Point {pntLat = 49.4, pntLon = 8.65, pntEle = Just 110.0, pntTime = Nothing}))
  , (ETIH, ICAOData (Point {pntLat = 49.21666666666667, pntLon = 11.833333333333334, pntEle = Just 442.0, pntTime = Nothing}))
  , (ETIK, ICAOData (Point {pntLat = 49.46666666666667, pntLon = 10.383333333333333, pntEle = Just 325.0, pntTime = Nothing}))
  , (ETIN, ICAOData (Point {pntLat = 49.75, pntLon = 10.2, pntEle = Just 210.0, pntTime = Nothing}))
  , (ETME, ICAOData (Point {pntLat = 54.63333333333333, pntLon = 9.35, pntEle = Just 20.0, pntTime = Nothing}))
  , (ETMK, ICAOData (Point {pntLat = 54.38333333333333, pntLon = 10.15, pntEle = Just 31.0, pntTime = Nothing}))
  , (ETMN, ICAOData (Point {pntLat = 53.766666666666666, pntLon = 8.666666666666666, pntEle = Just 23.0, pntTime = Nothing}))
  , (ETND, ICAOData (Point {pntLat = 52.583333333333336, pntLon = 8.35, pntEle = Just 39.0, pntTime = Nothing}))
  , (ETNG, ICAOData (Point {pntLat = 50.96666666666667, pntLon = 6.05, pntEle = Just 90.0, pntTime = Nothing}))
  , (ETNH, ICAOData (Point {pntLat = 54.31666666666667, pntLon = 9.533333333333333, pntEle = Just 12.0, pntTime = Nothing}))
  , (ETNJ, ICAOData (Point {pntLat = 53.53333333333333, pntLon = 7.883333333333333, pntEle = Just 7.0, pntTime = Nothing}))
  , (ETNL, ICAOData (Point {pntLat = 53.916666666666664, pntLon = 12.283333333333333, pntEle = Just 40.0, pntTime = Nothing}))
  , (ETNN, ICAOData (Point {pntLat = 50.833333333333336, pntLon = 6.666666666666667, pntEle = Just 118.0, pntTime = Nothing}))
  , (ETNP, ICAOData (Point {pntLat = 52.333333333333336, pntLon = 7.533333333333333, pntEle = Just 39.0, pntTime = Nothing}))
  , (ETNR, ICAOData (Point {pntLat = 51.666666666666664, pntLon = 14.633333333333333, pntEle = Just 100.0, pntTime = Nothing}))
  , (ETNS, ICAOData (Point {pntLat = 54.46666666666667, pntLon = 9.516666666666667, pntEle = Just 22.0, pntTime = Nothing}))
  , (ETNT, ICAOData (Point {pntLat = 53.55, pntLon = 7.666666666666667, pntEle = Just 8.0, pntTime = Nothing}))
  , (ETNU, ICAOData (Point {pntLat = 53.6, pntLon = 13.316666666666666, pntEle = Just 71.0, pntTime = Nothing}))
  , (ETNW, ICAOData (Point {pntLat = 52.45, pntLon = 9.433333333333334, pntEle = Just 57.0, pntTime = Nothing}))
  , (ETOR, ICAOData (Point {pntLat = 49.56666666666667, pntLon = 8.466666666666667, pntEle = Just 96.0, pntTime = Nothing}))
  , (ETOU, ICAOData (Point {pntLat = 50.05, pntLon = 8.333333333333334, pntEle = Just 140.0, pntTime = Nothing}))
  , (ETSA, ICAOData (Point {pntLat = 48.06666666666667, pntLon = 10.9, pntEle = Just 623.0, pntTime = Nothing}))
  , (ETSB, ICAOData (Point {pntLat = 50.166666666666664, pntLon = 7.066666666666666, pntEle = Just 478.0, pntTime = Nothing}))
  , (ETSE, ICAOData (Point {pntLat = 48.31666666666667, pntLon = 11.95, pntEle = Just 462.0, pntTime = Nothing}))
  , (ETSF, ICAOData (Point {pntLat = 48.2, pntLon = 11.266666666666667, pntEle = Just 519.0, pntTime = Nothing}))
  , (ETSH, ICAOData (Point {pntLat = 51.766666666666666, pntLon = 13.183333333333334, pntEle = Just 79.0, pntTime = Nothing}))
  , (ETSI, ICAOData (Point {pntLat = 48.71666666666667, pntLon = 11.533333333333333, pntEle = Just 367.0, pntTime = Nothing}))
  , (ETSL, ICAOData (Point {pntLat = 48.18333333333333, pntLon = 10.866666666666667, pntEle = Just 555.0, pntTime = Nothing}))
  , (ETSM, ICAOData (Point {pntLat = 47.983333333333334, pntLon = 10.233333333333333, pntEle = Just 634.0, pntTime = Nothing}))
  , (ETSN, ICAOData (Point {pntLat = 48.71666666666667, pntLon = 11.216666666666667, pntEle = Just 380.0, pntTime = Nothing}))
  , (ETSP, ICAOData (Point {pntLat = 49.85, pntLon = 7.6, pntEle = Just 396.0, pntTime = Nothing}))
  , (ETUL, ICAOData (Point {pntLat = 51.6, pntLon = 6.15, pntEle = Just 32.0, pntTime = Nothing}))
  , (ETUN, ICAOData (Point {pntLat = 52.45, pntLon = 7.166666666666667, pntEle = Just 26.0, pntTime = Nothing}))
  , (ETUO, ICAOData (Point {pntLat = 51.916666666666664, pntLon = 8.3, pntEle = Just 72.0, pntTime = Nothing}))
  , (ETUR, ICAOData (Point {pntLat = 51.2, pntLon = 6.133333333333334, pntEle = Just 73.0, pntTime = Nothing}))
  , (EYVI, ICAOData (Point {pntLat = 54.63333333333333, pntLon = 25.1, pntEle = Just 162.0, pntTime = Nothing}))
  , (FAAB, ICAOData (Point {pntLat = -28.566666666666666, pntLon = 16.533333333333335, pntEle = Just 21.0, pntTime = Nothing}))
  , (FAAN, ICAOData (Point {pntLat = -30.716666666666665, pntLon = 26.716666666666665, pntEle = Just 1348.0, pntTime = Nothing}))
  , (FABL, ICAOData (Point {pntLat = -29.1, pntLon = 26.3, pntEle = Just 1354.0, pntTime = Nothing}))
  , (FABM, ICAOData (Point {pntLat = -28.25, pntLon = 28.333333333333332, pntEle = Just 1686.0, pntTime = Nothing}))
  , (FABY, ICAOData (Point {pntLat = -32.35, pntLon = 22.583333333333332, pntEle = Just 842.0, pntTime = Nothing}))
  , (FACL, ICAOData (Point {pntLat = -26.066666666666666, pntLon = 30.116666666666667, pntEle = Just 1693.0, pntTime = Nothing}))
  , (FACT, ICAOData (Point {pntLat = -33.983333333333334, pntLon = 18.6, pntEle = Just 42.0, pntTime = Nothing}))
  , (FACV, ICAOData (Point {pntLat = -31.466666666666665, pntLon = 19.766666666666666, pntEle = Just 975.0, pntTime = Nothing}))
  , (FADA, ICAOData (Point {pntLat = -30.65, pntLon = 24.016666666666666, pntEle = Just 1247.0, pntTime = Nothing}))
  , (FADN, ICAOData (Point {pntLat = -29.966666666666665, pntLon = 30.95, pntEle = Just 14.0, pntTime = Nothing}))
  , (FADY, ICAOData (Point {pntLat = -30.633333333333333, pntLon = 23.916666666666668, pntEle = Just 384.0, pntTime = Nothing}))
  , (FAEL, ICAOData (Point {pntLat = -33.03333333333333, pntLon = 27.833333333333332, pntEle = Just 125.0, pntTime = Nothing}))
  , (FAER, ICAOData (Point {pntLat = -23.716666666666665, pntLon = 27.683333333333334, pntEle = Just 851.0, pntTime = Nothing}))
  , (FAFF, ICAOData (Point {pntLat = -27.266666666666666, pntLon = 28.5, pntEle = Just 1499.0, pntTime = Nothing}))
  , (FAFR, ICAOData (Point {pntLat = -31.916666666666668, pntLon = 21.516666666666666, pntEle = Just 1264.0, pntTime = Nothing}))
  , (FAGB, ICAOData (Point {pntLat = -22.466666666666665, pntLon = 18.966666666666665, pntEle = Just 1440.0, pntTime = Nothing}))
  , (FAGE, ICAOData (Point {pntLat = -40.35, pntLon = -9.883333333333333, pntEle = Just 54.0, pntTime = Nothing}))
  , (FAGG, ICAOData (Point {pntLat = -34.0, pntLon = 22.383333333333333, pntEle = Just 193.0, pntTime = Nothing}))
  , (FAGR, ICAOData (Point {pntLat = -32.25, pntLon = 24.533333333333335, pntEle = Just 752.0, pntTime = Nothing}))
  , (FAHS, ICAOData (Point {pntLat = -24.366666666666667, pntLon = 31.033333333333335, pntEle = Just 513.0, pntTime = Nothing}))
  , (FAIR, ICAOData (Point {pntLat = -25.916666666666668, pntLon = 28.216666666666665, pntEle = Just 1523.0, pntTime = Nothing}))
  , (FAJS, ICAOData (Point {pntLat = -26.133333333333333, pntLon = 28.233333333333334, pntEle = Just 1694.0, pntTime = Nothing}))
  , (FAKD, ICAOData (Point {pntLat = -26.866666666666667, pntLon = 26.716666666666665, pntEle = Just 1355.0, pntTime = Nothing}))
  , (FAKM, ICAOData (Point {pntLat = -28.8, pntLon = 24.766666666666666, pntEle = Just 1198.0, pntTime = Nothing}))
  , (FALW, ICAOData (Point {pntLat = -32.96666666666667, pntLon = 18.166666666666668, pntEle = Just 31.0, pntTime = Nothing}))
  , (FALY, ICAOData (Point {pntLat = -28.566666666666666, pntLon = 29.766666666666666, pntEle = Just 1078.0, pntTime = Nothing}))
  , (FAMB, ICAOData (Point {pntLat = -31.483333333333334, pntLon = 25.033333333333335, pntEle = Just 1270.0, pntTime = Nothing}))
  , (FAME, ICAOData (Point {pntLat = -46.88333333333333, pntLon = 37.86666666666667, pntEle = Just 22.0, pntTime = Nothing}))
  , (FAMM, ICAOData (Point {pntLat = -25.783333333333335, pntLon = 25.533333333333335, pntEle = Just 1281.0, pntTime = Nothing}))
  , (FAMO, ICAOData (Point {pntLat = -34.18333333333333, pntLon = 22.15, pntEle = Just 59.0, pntTime = Nothing}))
  , (FANS, ICAOData (Point {pntLat = -25.433333333333334, pntLon = 30.983333333333334, pntEle = Just 671.0, pntTime = Nothing}))
  , (FAOH, ICAOData (Point {pntLat = -33.56666666666667, pntLon = 22.216666666666665, pntEle = Just 359.0, pntTime = Nothing}))
  , (FAPB, ICAOData (Point {pntLat = -23.866666666666667, pntLon = 29.45, pntEle = Just 1228.0, pntTime = Nothing}))
  , (FAPE, ICAOData (Point {pntLat = -33.983333333333334, pntLon = 25.6, pntEle = Just 61.0, pntTime = Nothing}))
  , (FAPH, ICAOData (Point {pntLat = -23.933333333333334, pntLon = 31.15, pntEle = Just 427.0, pntTime = Nothing}))
  , (FAPJ, ICAOData (Point {pntLat = -31.633333333333333, pntLon = 29.55, pntEle = Just 47.0, pntTime = Nothing}))
  , (FAPR, ICAOData (Point {pntLat = -25.733333333333334, pntLon = 28.183333333333334, pntEle = Just 1330.0, pntTime = Nothing}))
  , (FAQT, ICAOData (Point {pntLat = -31.9, pntLon = 26.866666666666667, pntEle = Just 1094.0, pntTime = Nothing}))
  , (FARB, ICAOData (Point {pntLat = -28.8, pntLon = 32.1, pntEle = Just 47.0, pntTime = Nothing}))
  , (FASB, ICAOData (Point {pntLat = -29.666666666666668, pntLon = 17.866666666666667, pntEle = Just 1006.0, pntTime = Nothing}))
  , (FATC, ICAOData (Point {pntLat = -37.05, pntLon = -12.316666666666666, pntEle = Just 23.0, pntTime = Nothing}))
  , (FAUP, ICAOData (Point {pntLat = -28.4, pntLon = 21.266666666666666, pntEle = Just 839.0, pntTime = Nothing}))
  , (FAUT, ICAOData (Point {pntLat = -31.533333333333335, pntLon = 28.666666666666668, pntEle = Just 742.0, pntTime = Nothing}))
  , (FAVB, ICAOData (Point {pntLat = -26.95, pntLon = 24.633333333333333, pntEle = Just 1234.0, pntTime = Nothing}))
  , (FAVR, ICAOData (Point {pntLat = -31.666666666666668, pntLon = 18.5, pntEle = Just 34.0, pntTime = Nothing}))
  , (FAWK, ICAOData (Point {pntLat = -25.833333333333332, pntLon = 28.216666666666665, pntEle = Just 1498.0, pntTime = Nothing}))
  , (FAWM, ICAOData (Point {pntLat = -28.0, pntLon = 26.666666666666668, pntEle = Just 1338.0, pntTime = Nothing}))
  , (FBFT, ICAOData (Point {pntLat = -21.216666666666665, pntLon = 27.5, pntEle = Just 1001.0, pntTime = Nothing}))
  , (FBGZ, ICAOData (Point {pntLat = -21.7, pntLon = 21.65, pntEle = Just 1131.0, pntTime = Nothing}))
  , (FBJW, ICAOData (Point {pntLat = -24.6, pntLon = 24.666666666666668, pntEle = Just 1189.0, pntTime = Nothing}))
  , (FBKE, ICAOData (Point {pntLat = -17.816666666666666, pntLon = 25.15, pntEle = Just 968.0, pntTime = Nothing}))
  , (FBLT, ICAOData (Point {pntLat = -21.416666666666668, pntLon = 25.6, pntEle = Just 986.0, pntTime = Nothing}))
  , (FBMN, ICAOData (Point {pntLat = -19.983333333333334, pntLon = 23.416666666666668, pntEle = Just 945.0, pntTime = Nothing}))
  , (FBSK, ICAOData (Point {pntLat = -24.216666666666665, pntLon = 25.916666666666668, pntEle = Just 1005.0, pntTime = Nothing}))
  , (FBSN, ICAOData (Point {pntLat = -20.466666666666665, pntLon = 25.983333333333334, pntEle = Just 903.0, pntTime = Nothing}))
  , (FBSW, ICAOData (Point {pntLat = -18.366666666666667, pntLon = 21.85, pntEle = Just 1032.0, pntTime = Nothing}))
  , (FBTE, ICAOData (Point {pntLat = -24.016666666666666, pntLon = 21.883333333333333, pntEle = Just 1118.0, pntTime = Nothing}))
  , (FBTS, ICAOData (Point {pntLat = -26.05, pntLon = 22.45, pntEle = Just 960.0, pntTime = Nothing}))
  , (FCBB, ICAOData (Point {pntLat = -4.25, pntLon = 15.25, pntEle = Just 319.0, pntTime = Nothing}))
  , (FCBD, ICAOData (Point {pntLat = -2.533333333333333, pntLon = 14.766666666666667, pntEle = Just 791.0, pntTime = Nothing}))
  , (FCBM, ICAOData (Point {pntLat = -3.9833333333333334, pntLon = 13.916666666666666, pntEle = Just 509.0, pntTime = Nothing}))
  , (FCBO, ICAOData (Point {pntLat = -2.6166666666666667, pntLon = 16.216666666666665, pntEle = Just 311.0, pntTime = Nothing}))
  , (FCBS, ICAOData (Point {pntLat = -3.6833333333333336, pntLon = 13.35, pntEle = Just 530.0, pntTime = Nothing}))
  , (FCOG, ICAOData (Point {pntLat = -1.8666666666666667, pntLon = 15.866666666666667, pntEle = Just 476.0, pntTime = Nothing}))
  , (FCOI, ICAOData (Point {pntLat = 1.6166666666666667, pntLon = 18.066666666666666, pntEle = Just 335.0, pntTime = Nothing}))
  , (FCOM, ICAOData (Point {pntLat = -1.6666666666666666e-2, pntLon = 15.583333333333334, pntEle = Just 394.0, pntTime = Nothing}))
  , (FCOS, ICAOData (Point {pntLat = 2.066666666666667, pntLon = 14.133333333333333, pntEle = Just 549.0, pntTime = Nothing}))
  , (FCOU, ICAOData (Point {pntLat = 1.6166666666666667, pntLon = 16.05, pntEle = Just 352.0, pntTime = Nothing}))
  , (FCPA, ICAOData (Point {pntLat = -3.4833333333333334, pntLon = 12.616666666666667, pntEle = Just 160.0, pntTime = Nothing}))
  , (FCPL, ICAOData (Point {pntLat = -4.2, pntLon = 12.7, pntEle = Just 329.0, pntTime = Nothing}))
  , (FCPP, ICAOData (Point {pntLat = -4.816666666666666, pntLon = 11.9, pntEle = Just 17.0, pntTime = Nothing}))
  , (FDMS, ICAOData (Point {pntLat = -26.533333333333335, pntLon = 31.3, pntEle = Just 641.0, pntTime = Nothing}))
  , (FEFA, ICAOData (Point {pntLat = 5.05, pntLon = 21.2, pntEle = Just 447.0, pntTime = Nothing}))
  , (FEFB, ICAOData (Point {pntLat = 5.4, pntLon = 26.5, pntEle = Just 650.0, pntTime = Nothing}))
  , (FEFF, ICAOData (Point {pntLat = 4.4, pntLon = 18.516666666666666, pntEle = Just 365.0, pntTime = Nothing}))
  , (FEFG, ICAOData (Point {pntLat = 4.733333333333333, pntLon = 22.833333333333332, pntEle = Just 499.0, pntTime = Nothing}))
  , (FEFI, ICAOData (Point {pntLat = 10.283333333333333, pntLon = 22.783333333333335, pntEle = Just 463.0, pntTime = Nothing}))
  , (FEFL, ICAOData (Point {pntLat = 5.266666666666667, pntLon = 17.633333333333333, pntEle = Just 673.0, pntTime = Nothing}))
  , (FEFM, ICAOData (Point {pntLat = 5.85, pntLon = 20.65, pntEle = Just 474.0, pntTime = Nothing}))
  , (FEFN, ICAOData (Point {pntLat = 8.4, pntLon = 20.65, pntEle = Just 510.0, pntTime = Nothing}))
  , (FEFO, ICAOData (Point {pntLat = 5.966666666666667, pntLon = 15.633333333333333, pntEle = Just 1019.0, pntTime = Nothing}))
  , (FEFR, ICAOData (Point {pntLat = 6.533333333333333, pntLon = 21.983333333333334, pntEle = Just 583.0, pntTime = Nothing}))
  , (FEFS, ICAOData (Point {pntLat = 6.483333333333333, pntLon = 17.433333333333334, pntEle = Just 464.0, pntTime = Nothing}))
  , (FEFT, ICAOData (Point {pntLat = 4.25, pntLon = 15.8, pntEle = Just 582.0, pntTime = Nothing}))
  , (FEFY, ICAOData (Point {pntLat = 6.5, pntLon = 23.266666666666666, pntEle = Just 601.0, pntTime = Nothing}))
  , (FGSL, ICAOData (Point {pntLat = 3.75, pntLon = 8.766666666666667, pntEle = Just 56.0, pntTime = Nothing}))
  , (FHAW, ICAOData (Point {pntLat = -7.966666666666667, pntLon = -14.4, pntEle = Just 86.0, pntTime = Nothing}))
  , (FIMP, ICAOData (Point {pntLat = -20.433333333333334, pntLon = 57.666666666666664, pntEle = Just 55.0, pntTime = Nothing}))
  , (FIMR, ICAOData (Point {pntLat = -19.683333333333334, pntLon = 63.416666666666664, pntEle = Just 58.0, pntTime = Nothing}))
  , (FJDG, ICAOData (Point {pntLat = -7.3, pntLon = 72.4, pntEle = Just 3.0, pntTime = Nothing}))
  , (FKAB, ICAOData (Point {pntLat = 6.783333333333333, pntLon = 11.816666666666666, pntEle = Just 1110.0, pntTime = Nothing}))
  , (FKAF, ICAOData (Point {pntLat = 4.733333333333333, pntLon = 11.25, pntEle = Just 500.0, pntTime = Nothing}))
  , (FKAG, ICAOData (Point {pntLat = 3.966666666666667, pntLon = 13.2, pntEle = Just 693.0, pntTime = Nothing}))
  , (FKAL, ICAOData (Point {pntLat = 3.15, pntLon = 13.616666666666667, pntEle = Just 624.0, pntTime = Nothing}))
  , (FKAM, ICAOData (Point {pntLat = 6.533333333333333, pntLon = 14.366666666666667, pntEle = Just 1027.0, pntTime = Nothing}))
  , (FKAN, ICAOData (Point {pntLat = 4.95, pntLon = 9.933333333333334, pntEle = Just 816.0, pntTime = Nothing}))
  , (FKAO, ICAOData (Point {pntLat = 5.6, pntLon = 14.066666666666666, pntEle = Just 815.0, pntTime = Nothing}))
  , (FKAY, ICAOData (Point {pntLat = 5.55, pntLon = 12.366666666666667, pntEle = Just 1027.0, pntTime = Nothing}))
  , (FKKA, ICAOData (Point {pntLat = 10.45, pntLon = 14.25, pntEle = Just 423.0, pntTime = Nothing}))
  , (FKKB, ICAOData (Point {pntLat = 2.95, pntLon = 9.9, pntEle = Just 10.0, pntTime = Nothing}))
  , (FKKC, ICAOData (Point {pntLat = 4.083333333333333, pntLon = 9.366666666666667, pntEle = Just 52.0, pntTime = Nothing}))
  , (FKKD, ICAOData (Point {pntLat = 4.0, pntLon = 9.733333333333333, pntEle = Just 10.0, pntTime = Nothing}))
  , (FKKF, ICAOData (Point {pntLat = 5.716666666666667, pntLon = 9.283333333333333, pntEle = Just 126.0, pntTime = Nothing}))
  , (FKKI, ICAOData (Point {pntLat = 4.466666666666667, pntLon = 14.366666666666667, pntEle = Just 656.0, pntTime = Nothing}))
  , (FKKM, ICAOData (Point {pntLat = 5.65, pntLon = 10.75, pntEle = Just 1208.0, pntTime = Nothing}))
  , (FKKN, ICAOData (Point {pntLat = 7.35, pntLon = 13.566666666666666, pntEle = Just 1114.0, pntTime = Nothing}))
  , (FKKR, ICAOData (Point {pntLat = 9.333333333333334, pntLon = 13.383333333333333, pntEle = Just 242.0, pntTime = Nothing}))
  , (FKKV, ICAOData (Point {pntLat = 6.05, pntLon = 10.116666666666667, pntEle = Just 1239.0, pntTime = Nothing}))
  , (FKYS, ICAOData (Point {pntLat = 3.8333333333333335, pntLon = 11.516666666666667, pntEle = Just 751.0, pntTime = Nothing}))
  , (FLBA, ICAOData (Point {pntLat = -8.85, pntLon = 31.333333333333332, pntEle = Just 1673.0, pntTime = Nothing}))
  , (FLCH, ICAOData (Point {pntLat = -16.833333333333332, pntLon = 27.066666666666666, pntEle = Just 1213.0, pntTime = Nothing}))
  , (FLCP, ICAOData (Point {pntLat = -13.55, pntLon = 32.583333333333336, pntEle = Just 1028.0, pntTime = Nothing}))
  , (FLIK, ICAOData (Point {pntLat = -10.116666666666667, pntLon = 32.63333333333333, pntEle = Just 1360.0, pntTime = Nothing}))
  , (FLKB, ICAOData (Point {pntLat = -9.8, pntLon = 29.083333333333332, pntEle = Just 1323.0, pntTime = Nothing}))
  , (FLKO, ICAOData (Point {pntLat = -14.8, pntLon = 24.8, pntEle = Just 1213.0, pntTime = Nothing}))
  , (FLKS, ICAOData (Point {pntLat = -10.216666666666667, pntLon = 31.133333333333333, pntEle = Just 1382.0, pntTime = Nothing}))
  , (FLKW, ICAOData (Point {pntLat = -14.45, pntLon = 28.466666666666665, pntEle = Just 1206.0, pntTime = Nothing}))
  , (FLLC, ICAOData (Point {pntLat = -14.45, pntLon = 28.466666666666665, pntEle = Just 1280.0, pntTime = Nothing}))
  , (FLLD, ICAOData (Point {pntLat = -12.283333333333333, pntLon = 33.2, pntEle = Just 1143.0, pntTime = Nothing}))
  , (FLLI, ICAOData (Point {pntLat = -17.816666666666666, pntLon = 25.816666666666666, pntEle = Just 985.0, pntTime = Nothing}))
  , (FLLS, ICAOData (Point {pntLat = -15.316666666666666, pntLon = 28.45, pntEle = Just 1152.0, pntTime = Nothing}))
  , (FLMA, ICAOData (Point {pntLat = -11.1, pntLon = 28.85, pntEle = Just 1382.0, pntTime = Nothing}))
  , (FLMG, ICAOData (Point {pntLat = -15.25, pntLon = 23.15, pntEle = Just 1052.0, pntTime = Nothing}))
  , (FLMP, ICAOData (Point {pntLat = -11.9, pntLon = 31.433333333333334, pntEle = Just 1400.0, pntTime = Nothing}))
  , (FLMW, ICAOData (Point {pntLat = -11.75, pntLon = 24.433333333333334, pntEle = Just 1361.0, pntTime = Nothing}))
  , (FLND, ICAOData (Point {pntLat = -13.0, pntLon = 28.65, pntEle = Just 1269.0, pntTime = Nothing}))
  , (FLPA, ICAOData (Point {pntLat = -13.533333333333333, pntLon = 25.85, pntEle = Just 1234.0, pntTime = Nothing}))
  , (FLPE, ICAOData (Point {pntLat = -14.25, pntLon = 31.283333333333335, pntEle = Just 1035.0, pntTime = Nothing}))
  , (FLPO, ICAOData (Point {pntLat = -13.6, pntLon = 24.2, pntEle = Just 1075.0, pntTime = Nothing}))
  , (FLSE, ICAOData (Point {pntLat = -13.233333333333333, pntLon = 30.216666666666665, pntEle = Just 1384.0, pntTime = Nothing}))
  , (FLSN, ICAOData (Point {pntLat = -16.1, pntLon = 23.266666666666666, pntEle = Just 1027.0, pntTime = Nothing}))
  , (FLSS, ICAOData (Point {pntLat = -17.466666666666665, pntLon = 24.3, pntEle = Just 949.0, pntTime = Nothing}))
  , (FLSW, ICAOData (Point {pntLat = -12.183333333333334, pntLon = 26.383333333333333, pntEle = Just 1386.0, pntTime = Nothing}))
  , (FLZB, ICAOData (Point {pntLat = -13.533333333333333, pntLon = 23.116666666666667, pntEle = Just 1077.0, pntTime = Nothing}))
  , (FMCH, ICAOData (Point {pntLat = -11.533333333333333, pntLon = 43.266666666666666, pntEle = Just 29.0, pntTime = Nothing}))
  , (FMCV, ICAOData (Point {pntLat = -12.116666666666667, pntLon = 44.43333333333333, pntEle = Just 19.0, pntTime = Nothing}))
  , (FMCZ, ICAOData (Point {pntLat = -12.8, pntLon = 45.28333333333333, pntEle = Just 8.0, pntTime = Nothing}))
  , (FMEE, ICAOData (Point {pntLat = -20.883333333333333, pntLon = 55.516666666666666, pntEle = Just 21.0, pntTime = Nothing}))
  , (FMEP, ICAOData (Point {pntLat = -21.333333333333332, pntLon = 55.483333333333334, pntEle = Just 52.0, pntTime = Nothing}))
  , (FMME, ICAOData (Point {pntLat = -19.816666666666666, pntLon = 47.06666666666667, pntEle = Just 1523.0, pntTime = Nothing}))
  , (FMMH, ICAOData (Point {pntLat = -19.833333333333332, pntLon = 48.8, pntEle = Just 5.0, pntTime = Nothing}))
  , (FMMI, ICAOData (Point {pntLat = -18.8, pntLon = 47.483333333333334, pntEle = Just 1279.0, pntTime = Nothing}))
  , (FMMO, ICAOData (Point {pntLat = -18.05, pntLon = 44.03333333333333, pntEle = Just 23.0, pntTime = Nothing}))
  , (FMMS, ICAOData (Point {pntLat = -17.083333333333332, pntLon = 49.81666666666667, pntEle = Just 2.0, pntTime = Nothing}))
  , (FMMT, ICAOData (Point {pntLat = -18.116666666666667, pntLon = 49.4, pntEle = Just 5.0, pntTime = Nothing}))
  , (FMMV, ICAOData (Point {pntLat = -20.283333333333335, pntLon = 44.31666666666667, pntEle = Just 7.0, pntTime = Nothing}))
  , (FMNA, ICAOData (Point {pntLat = -12.35, pntLon = 49.3, pntEle = Just 114.0, pntTime = Nothing}))
  , (FMND, ICAOData (Point {pntLat = -14.65, pntLon = 49.61666666666667, pntEle = Just 473.0, pntTime = Nothing}))
  , (FMNH, ICAOData (Point {pntLat = -14.883333333333333, pntLon = 50.25, pntEle = Just 87.0, pntTime = Nothing}))
  , (FMNL, ICAOData (Point {pntLat = -14.633333333333333, pntLon = 47.766666666666666, pntEle = Just 105.0, pntTime = Nothing}))
  , (FMNM, ICAOData (Point {pntLat = -15.666666666666666, pntLon = 46.35, pntEle = Just 26.0, pntTime = Nothing}))
  , (FMNN, ICAOData (Point {pntLat = -13.316666666666666, pntLon = 48.31666666666667, pntEle = Just 10.0, pntTime = Nothing}))
  , (FMNQ, ICAOData (Point {pntLat = -16.75, pntLon = 44.483333333333334, pntEle = Just 38.0, pntTime = Nothing}))
  , (FMNS, ICAOData (Point {pntLat = -14.283333333333333, pntLon = 50.166666666666664, pntEle = Just 5.0, pntTime = Nothing}))
  , (FMNV, ICAOData (Point {pntLat = -13.366666666666667, pntLon = 50.0, pntEle = Just 6.0, pntTime = Nothing}))
  , (FMSD, ICAOData (Point {pntLat = -25.033333333333335, pntLon = 46.95, pntEle = Just 8.0, pntTime = Nothing}))
  , (FMSF, ICAOData (Point {pntLat = -21.45, pntLon = 47.1, pntEle = Just 1115.0, pntTime = Nothing}))
  , (FMSG, ICAOData (Point {pntLat = -22.8, pntLon = 47.833333333333336, pntEle = Just 8.0, pntTime = Nothing}))
  , (FMSM, ICAOData (Point {pntLat = -21.2, pntLon = 48.36666666666667, pntEle = Just 5.0, pntTime = Nothing}))
  , (FMSO, ICAOData (Point {pntLat = -22.55, pntLon = 45.4, pntEle = Just 824.0, pntTime = Nothing}))
  , (FMSR, ICAOData (Point {pntLat = -21.75, pntLon = 43.36666666666667, pntEle = Just 5.0, pntTime = Nothing}))
  , (FMST, ICAOData (Point {pntLat = -23.383333333333333, pntLon = 43.733333333333334, pntEle = Just 8.0, pntTime = Nothing}))
  , (FNBG, ICAOData (Point {pntLat = -12.583333333333334, pntLon = 13.416666666666666, pntEle = Just 33.0, pntTime = Nothing}))
  , (FNCA, ICAOData (Point {pntLat = -5.55, pntLon = 12.183333333333334, pntEle = Just 25.0, pntTime = Nothing}))
  , (FNHU, ICAOData (Point {pntLat = -12.8, pntLon = 15.75, pntEle = Just 1710.0, pntTime = Nothing}))
  , (FNKU, ICAOData (Point {pntLat = -12.383333333333333, pntLon = 16.95, pntEle = Just 1701.0, pntTime = Nothing}))
  , (FNLU, ICAOData (Point {pntLat = -8.85, pntLon = 13.233333333333333, pntEle = Just 74.0, pntTime = Nothing}))
  , (FOGM, ICAOData (Point {pntLat = -1.8666666666666667, pntLon = 11.016666666666667, pntEle = Just 88.0, pntTime = Nothing}))
  , (FOGR, ICAOData (Point {pntLat = -0.7166666666666667, pntLon = 10.233333333333333, pntEle = Just 27.0, pntTime = Nothing}))
  , (FOOB, ICAOData (Point {pntLat = 2.0833333333333335, pntLon = 11.483333333333333, pntEle = Just 600.0, pntTime = Nothing}))
  , (FOOC, ICAOData (Point {pntLat = 1.0, pntLon = 9.6, pntEle = Just 12.0, pntTime = Nothing}))
  , (FOOD, ICAOData (Point {pntLat = -1.5333333333333332, pntLon = 13.266666666666667, pntEle = Just 572.0, pntTime = Nothing}))
  , (FOOE, ICAOData (Point {pntLat = 1.0166666666666666, pntLon = 13.933333333333334, pntEle = Just 499.0, pntTime = Nothing}))
  , (FOOG, ICAOData (Point {pntLat = -0.7, pntLon = 8.75, pntEle = Just 3.0, pntTime = Nothing}))
  , (FOOK, ICAOData (Point {pntLat = 0.5666666666666667, pntLon = 12.866666666666667, pntEle = Just 509.0, pntTime = Nothing}))
  , (FOOL, ICAOData (Point {pntLat = 0.45, pntLon = 9.416666666666666, pntEle = Just 12.0, pntTime = Nothing}))
  , (FOOM, ICAOData (Point {pntLat = 0.7833333333333333, pntLon = 11.533333333333333, pntEle = Just 583.0, pntTime = Nothing}))
  , (FOOR, ICAOData (Point {pntLat = -0.8333333333333334, pntLon = 12.716666666666667, pntEle = Just 483.0, pntTime = Nothing}))
  , (FOOT, ICAOData (Point {pntLat = -2.85, pntLon = 11.016666666666667, pntEle = Just 83.0, pntTime = Nothing}))
  , (FOOY, ICAOData (Point {pntLat = -3.4166666666666665, pntLon = 10.65, pntEle = Just 31.0, pntTime = Nothing}))
  , (FPPR, ICAOData (Point {pntLat = 1.65, pntLon = 7.416666666666667, pntEle = Just 3.0, pntTime = Nothing}))
  , (FPST, ICAOData (Point {pntLat = 0.38333333333333336, pntLon = 6.716666666666667, pntEle = Just 8.0, pntTime = Nothing}))
  , (FQBR, ICAOData (Point {pntLat = -19.8, pntLon = 34.9, pntEle = Just 8.0, pntTime = Nothing}))
  , (FQCB, ICAOData (Point {pntLat = -14.816666666666666, pntLon = 36.53333333333333, pntEle = Just 606.0, pntTime = Nothing}))
  , (FQCH, ICAOData (Point {pntLat = -19.116666666666667, pntLon = 33.46666666666667, pntEle = Just 731.0, pntTime = Nothing}))
  , (FQIN, ICAOData (Point {pntLat = -23.866666666666667, pntLon = 35.38333333333333, pntEle = Just 14.0, pntTime = Nothing}))
  , (FQLC, ICAOData (Point {pntLat = -13.3, pntLon = 35.233333333333334, pntEle = Just 1364.0, pntTime = Nothing}))
  , (FQLU, ICAOData (Point {pntLat = -15.033333333333333, pntLon = 40.666666666666664, pntEle = Just 10.0, pntTime = Nothing}))
  , (FQMA, ICAOData (Point {pntLat = -25.916666666666668, pntLon = 32.56666666666667, pntEle = Just 39.0, pntTime = Nothing}))
  , (FQMP, ICAOData (Point {pntLat = -11.35, pntLon = 40.36666666666667, pntEle = Just 27.0, pntTime = Nothing}))
  , (FQNP, ICAOData (Point {pntLat = -15.1, pntLon = 39.28333333333333, pntEle = Just 438.0, pntTime = Nothing}))
  , (FQPB, ICAOData (Point {pntLat = -12.983333333333333, pntLon = 40.53333333333333, pntEle = Just 101.0, pntTime = Nothing}))
  , (FQQL, ICAOData (Point {pntLat = -17.883333333333333, pntLon = 36.88333333333333, pntEle = Just 6.0, pntTime = Nothing}))
  , (FQTE, ICAOData (Point {pntLat = -16.183333333333334, pntLon = 33.583333333333336, pntEle = Just 149.0, pntTime = Nothing}))
  , (FQVL, ICAOData (Point {pntLat = -22.0, pntLon = 35.31666666666667, pntEle = Just 20.0, pntTime = Nothing}))
  , (FQXA, ICAOData (Point {pntLat = -25.05, pntLon = 33.63333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (FSIA, ICAOData (Point {pntLat = -4.666666666666667, pntLon = 55.516666666666666, pntEle = Just 3.0, pntTime = Nothing}))
  , (FSSS, ICAOData (Point {pntLat = -4.683333333333334, pntLon = 55.53333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (FTTA, ICAOData (Point {pntLat = 9.15, pntLon = 18.383333333333333, pntEle = Just 365.0, pntTime = Nothing}))
  , (FTTC, ICAOData (Point {pntLat = 13.85, pntLon = 20.85, pntEle = Just 545.0, pntTime = Nothing}))
  , (FTTD, ICAOData (Point {pntLat = 8.616666666666667, pntLon = 16.066666666666666, pntEle = Just 428.0, pntTime = Nothing}))
  , (FTTJ, ICAOData (Point {pntLat = 12.133333333333333, pntLon = 15.033333333333333, pntEle = Just 295.0, pntTime = Nothing}))
  , (FTTK, ICAOData (Point {pntLat = 12.383333333333333, pntLon = 17.05, pntEle = Just 300.0, pntTime = Nothing}))
  , (FTTL, ICAOData (Point {pntLat = 13.433333333333334, pntLon = 14.733333333333333, pntEle = Just 291.0, pntTime = Nothing}))
  , (FTTN, ICAOData (Point {pntLat = 11.033333333333333, pntLon = 20.283333333333335, pntEle = Just 433.0, pntTime = Nothing}))
  , (FTTP, ICAOData (Point {pntLat = 9.366666666666667, pntLon = 14.916666666666666, pntEle = Just 467.0, pntTime = Nothing}))
  , (FTTY, ICAOData (Point {pntLat = 18.0, pntLon = 19.166666666666668, pntEle = Just 235.0, pntTime = Nothing}))
  , (FVBB, ICAOData (Point {pntLat = -22.216666666666665, pntLon = 30.0, pntEle = Just 456.0, pntTime = Nothing}))
  , (FVBI, ICAOData (Point {pntLat = -17.616666666666667, pntLon = 27.333333333333332, pntEle = Just 620.0, pntTime = Nothing}))
  , (FVBU, ICAOData (Point {pntLat = -20.016666666666666, pntLon = 28.616666666666667, pntEle = Just 1326.0, pntTime = Nothing}))
  , (FVCH, ICAOData (Point {pntLat = -20.2, pntLon = 32.61666666666667, pntEle = Just 1131.0, pntTime = Nothing}))
  , (FVCZ, ICAOData (Point {pntLat = -21.016666666666666, pntLon = 31.583333333333332, pntEle = Just 429.0, pntTime = Nothing}))
  , (FVFA, ICAOData (Point {pntLat = -18.1, pntLon = 25.85, pntEle = Just 1061.0, pntTime = Nothing}))
  , (FVGO, ICAOData (Point {pntLat = -18.216666666666665, pntLon = 28.933333333333334, pntEle = Just 1282.0, pntTime = Nothing}))
  , (FVHA, ICAOData (Point {pntLat = -17.916666666666668, pntLon = 31.133333333333333, pntEle = Just 1479.0, pntTime = Nothing}))
  , (FVKA, ICAOData (Point {pntLat = -16.833333333333332, pntLon = 29.616666666666667, pntEle = Just 1343.0, pntTime = Nothing}))
  , (FVKB, ICAOData (Point {pntLat = -16.516666666666666, pntLon = 28.883333333333333, pntEle = Just 518.0, pntTime = Nothing}))
  , (FVMT, ICAOData (Point {pntLat = -17.416666666666668, pntLon = 32.21666666666667, pntEle = Just 1244.0, pntTime = Nothing}))
  , (FVMV, ICAOData (Point {pntLat = -20.066666666666666, pntLon = 30.866666666666667, pntEle = Just 1094.0, pntTime = Nothing}))
  , (FVRU, ICAOData (Point {pntLat = -18.533333333333335, pntLon = 32.13333333333333, pntEle = Just 1430.0, pntTime = Nothing}))
  , (FVTL, ICAOData (Point {pntLat = -19.45, pntLon = 29.85, pntEle = Just 1428.0, pntTime = Nothing}))
  , (FVWN, ICAOData (Point {pntLat = -18.633333333333333, pntLon = 27.0, pntEle = Just 1079.0, pntTime = Nothing}))
  , (FWCL, ICAOData (Point {pntLat = -15.683333333333334, pntLon = 34.96666666666667, pntEle = Just 766.0, pntTime = Nothing}))
  , (FWCT, ICAOData (Point {pntLat = -9.7, pntLon = 33.266666666666666, pntEle = Just 1278.0, pntTime = Nothing}))
  , (FWDZ, ICAOData (Point {pntLat = -14.316666666666666, pntLon = 34.266666666666666, pntEle = Just 1630.0, pntTime = Nothing}))
  , (FWKA, ICAOData (Point {pntLat = -9.95, pntLon = 33.88333333333333, pntEle = Just 529.0, pntTime = Nothing}))
  , (FWKI, ICAOData (Point {pntLat = -13.783333333333333, pntLon = 33.766666666666666, pntEle = Just 1229.0, pntTime = Nothing}))
  , (FWKK, ICAOData (Point {pntLat = -12.916666666666666, pntLon = 34.266666666666666, pntEle = Just 500.0, pntTime = Nothing}))
  , (FWMG, ICAOData (Point {pntLat = -14.433333333333334, pntLon = 35.25, pntEle = Just 485.0, pntTime = Nothing}))
  , (FWMY, ICAOData (Point {pntLat = -14.083333333333334, pntLon = 34.916666666666664, pntEle = Just 474.0, pntTime = Nothing}))
  , (FWMZ, ICAOData (Point {pntLat = -11.883333333333333, pntLon = 33.61666666666667, pntEle = Just 1349.0, pntTime = Nothing}))
  , (FWSM, ICAOData (Point {pntLat = -13.75, pntLon = 34.583333333333336, pntEle = Just 513.0, pntTime = Nothing}))
  , (FWUU, ICAOData (Point {pntLat = -11.45, pntLon = 34.016666666666666, pntEle = Just 1251.0, pntTime = Nothing}))
  , (FXMU, ICAOData (Point {pntLat = -29.45, pntLon = 27.55, pntEle = Just 1631.0, pntTime = Nothing}))
  , (FYGF, ICAOData (Point {pntLat = -19.6, pntLon = 18.116666666666667, pntEle = Just 1411.0, pntTime = Nothing}))
  , (FYKT, ICAOData (Point {pntLat = -26.533333333333335, pntLon = 18.116666666666667, pntEle = Just 1067.0, pntTime = Nothing}))
  , (FYRU, ICAOData (Point {pntLat = -17.916666666666668, pntLon = 19.766666666666666, pntEle = Just 1083.0, pntTime = Nothing}))
  , (FYWH, ICAOData (Point {pntLat = -22.483333333333334, pntLon = 17.466666666666665, pntEle = Just 1715.0, pntTime = Nothing}))
  , (FZAA, ICAOData (Point {pntLat = -4.383333333333334, pntLon = 15.433333333333334, pntEle = Just 309.0, pntTime = Nothing}))
  , (FZAG, ICAOData (Point {pntLat = -6.0, pntLon = 12.416666666666666, pntEle = Just 27.0, pntTime = Nothing}))
  , (FZAM, ICAOData (Point {pntLat = -5.8, pntLon = 13.433333333333334, pntEle = Just 355.0, pntTime = Nothing}))
  , (FZAN, ICAOData (Point {pntLat = -5.516666666666667, pntLon = 13.583333333333334, pntEle = Just 277.0, pntTime = Nothing}))
  , (FZBA, ICAOData (Point {pntLat = -1.9666666666666668, pntLon = 18.266666666666666, pntEle = Just 300.0, pntTime = Nothing}))
  , (FZBO, ICAOData (Point {pntLat = -3.3, pntLon = 17.35, pntEle = Just 324.0, pntTime = Nothing}))
  , (FZCA, ICAOData (Point {pntLat = -5.033333333333333, pntLon = 18.8, pntEle = Just 518.0, pntTime = Nothing}))
  , (FZCS, ICAOData (Point {pntLat = -4.916666666666667, pntLon = 17.066666666666666, pntEle = Just 563.0, pntTime = Nothing}))
  , (FZEA, ICAOData (Point {pntLat = 5.0e-2, pntLon = 18.266666666666666, pntEle = Just 345.0, pntTime = Nothing}))
  , (FZFK, ICAOData (Point {pntLat = 3.283333333333333, pntLon = 19.783333333333335, pntEle = Just 475.0, pntTime = Nothing}))
  , (FZGN, ICAOData (Point {pntLat = -0.21666666666666667, pntLon = 20.85, pntEle = Just 351.0, pntTime = Nothing}))
  , (FZIA, ICAOData (Point {pntLat = 0.5166666666666667, pntLon = 25.183333333333334, pntEle = Just 415.0, pntTime = Nothing}))
  , (FZMB, ICAOData (Point {pntLat = 0.13333333333333333, pntLon = 29.266666666666666, pntEle = Just 1840.0, pntTime = Nothing}))
  , (FZNA, ICAOData (Point {pntLat = -1.6833333333333333, pntLon = 29.233333333333334, pntEle = Just 1552.0, pntTime = Nothing}))
  , (FZNC, ICAOData (Point {pntLat = -1.1833333333333333, pntLon = 29.45, pntEle = Just 1275.0, pntTime = Nothing}))
  , (FZOA, ICAOData (Point {pntLat = -2.95, pntLon = 25.916666666666668, pntEle = Just 497.0, pntTime = Nothing}))
  , (FZQA, ICAOData (Point {pntLat = -11.666666666666666, pntLon = 27.483333333333334, pntEle = Just 1276.0, pntTime = Nothing}))
  , (FZQM, ICAOData (Point {pntLat = -10.716666666666667, pntLon = 25.45, pntEle = Just 1405.0, pntTime = Nothing}))
  , (FZRA, ICAOData (Point {pntLat = -7.283333333333333, pntLon = 27.433333333333334, pntEle = Just 614.0, pntTime = Nothing}))
  , (FZRF, ICAOData (Point {pntLat = -5.883333333333333, pntLon = 29.183333333333334, pntEle = Just 790.0, pntTime = Nothing}))
  , (FZRQ, ICAOData (Point {pntLat = -5.35, pntLon = 27.0, pntEle = Just 561.0, pntTime = Nothing}))
  , (FZSA, ICAOData (Point {pntLat = -8.633333333333333, pntLon = 25.25, pntEle = Just 1088.0, pntTime = Nothing}))
  , (FZUA, ICAOData (Point {pntLat = -5.883333333333333, pntLon = 22.416666666666668, pntEle = Just 654.0, pntTime = Nothing}))
  , (FZUK, ICAOData (Point {pntLat = -6.416666666666667, pntLon = 20.85, pntEle = Just 521.0, pntTime = Nothing}))
  , (FZVA, ICAOData (Point {pntLat = -3.4833333333333334, pntLon = 23.466666666666665, pntEle = Just 479.0, pntTime = Nothing}))
  , (FZVI, ICAOData (Point {pntLat = -4.966666666666667, pntLon = 23.433333333333334, pntEle = Just 424.0, pntTime = Nothing}))
  , (FZVS, ICAOData (Point {pntLat = -4.333333333333333, pntLon = 20.583333333333332, pntEle = Just 465.0, pntTime = Nothing}))
  , (FZWA, ICAOData (Point {pntLat = -6.166666666666667, pntLon = 23.616666666666667, pntEle = Just 633.0, pntTime = Nothing}))
  , (GABG, ICAOData (Point {pntLat = 11.416666666666666, pntLon = -7.5, pntEle = Just 350.0, pntTime = Nothing}))
  , (GABS, ICAOData (Point {pntLat = 12.533333333333333, pntLon = -7.95, pntEle = Just 380.0, pntTime = Nothing}))
  , (GAGO, ICAOData (Point {pntLat = 16.266666666666666, pntLon = -5.0e-2, pntEle = Just 265.0, pntTime = Nothing}))
  , (GAHB, ICAOData (Point {pntLat = 15.333333333333334, pntLon = -1.6833333333333333, pntEle = Just 287.0, pntTime = Nothing}))
  , (GAKA, ICAOData (Point {pntLat = 12.85, pntLon = -11.233333333333333, pntEle = Just 132.0, pntTime = Nothing}))
  , (GAKL, ICAOData (Point {pntLat = 18.433333333333334, pntLon = 1.35, pntEle = Just 458.0, pntTime = Nothing}))
  , (GAKO, ICAOData (Point {pntLat = 12.383333333333333, pntLon = -5.466666666666667, pntEle = Just 365.0, pntTime = Nothing}))
  , (GAKT, ICAOData (Point {pntLat = 13.066666666666666, pntLon = -9.466666666666667, pntEle = Just 333.0, pntTime = Nothing}))
  , (GAKY, ICAOData (Point {pntLat = 14.433333333333334, pntLon = -11.433333333333334, pntEle = Just 47.0, pntTime = Nothing}))
  , (GAMB, ICAOData (Point {pntLat = 14.516666666666667, pntLon = -4.1, pntEle = Just 276.0, pntTime = Nothing}))
  , (GAMK, ICAOData (Point {pntLat = 15.866666666666667, pntLon = 2.216666666666667, pntEle = Just 278.0, pntTime = Nothing}))
  , (GANK, ICAOData (Point {pntLat = 15.166666666666666, pntLon = -7.283333333333333, pntEle = Just 271.0, pntTime = Nothing}))
  , (GANR, ICAOData (Point {pntLat = 15.233333333333333, pntLon = -9.35, pntEle = Just 235.0, pntTime = Nothing}))
  , (GASG, ICAOData (Point {pntLat = 13.4, pntLon = -6.15, pntEle = Just 288.0, pntTime = Nothing}))
  , (GASK, ICAOData (Point {pntLat = 11.35, pntLon = -5.683333333333334, pntEle = Just 374.0, pntTime = Nothing}))
  , (GASN, ICAOData (Point {pntLat = 13.333333333333334, pntLon = -4.833333333333333, pntEle = Just 283.0, pntTime = Nothing}))
  , (GATB, ICAOData (Point {pntLat = 16.716666666666665, pntLon = -3.0, pntEle = Just 263.0, pntTime = Nothing}))
  , (GATS, ICAOData (Point {pntLat = 20.2, pntLon = 0.9833333333333333, pntEle = Just 494.0, pntTime = Nothing}))
  , (GBYD, ICAOData (Point {pntLat = 13.35, pntLon = -16.8, pntEle = Just 36.0, pntTime = Nothing}))
  , (GCFV, ICAOData (Point {pntLat = 28.45, pntLon = -13.866666666666667, pntEle = Just 22.0, pntTime = Nothing}))
  , (GCHI, ICAOData (Point {pntLat = 27.816666666666666, pntLon = -17.883333333333333, pntEle = Just 32.0, pntTime = Nothing}))
  , (GCLA, ICAOData (Point {pntLat = 28.616666666666667, pntLon = -17.75, pntEle = Just 29.0, pntTime = Nothing}))
  , (GCLP, ICAOData (Point {pntLat = 27.933333333333334, pntLon = -15.383333333333333, pntEle = Just 23.0, pntTime = Nothing}))
  , (GCRR, ICAOData (Point {pntLat = 28.95, pntLon = -13.6, pntEle = Just 14.0, pntTime = Nothing}))
  , (GCTS, ICAOData (Point {pntLat = 28.05, pntLon = -16.566666666666666, pntEle = Just 64.0, pntTime = Nothing}))
  , (GCXO, ICAOData (Point {pntLat = 28.466666666666665, pntLon = -16.316666666666666, pntEle = Just 632.0, pntTime = Nothing}))
  , (GEML, ICAOData (Point {pntLat = 35.28333333333333, pntLon = -2.95, pntEle = Just 47.0, pntTime = Nothing}))
  , (GFLL, ICAOData (Point {pntLat = 8.616666666666667, pntLon = -13.2, pntEle = Just 25.0, pntTime = Nothing}))
  , (GGBF, ICAOData (Point {pntLat = 12.166666666666666, pntLon = -14.666666666666666, pntEle = Just 42.0, pntTime = Nothing}))
  , (GGOV, ICAOData (Point {pntLat = 11.883333333333333, pntLon = -15.65, pntEle = Just 39.0, pntTime = Nothing}))
  , (GLRB, ICAOData (Point {pntLat = 6.25, pntLon = -10.35, pntEle = Just 18.0, pntTime = Nothing}))
  , (GMAA, ICAOData (Point {pntLat = 30.383333333333333, pntLon = -9.566666666666666, pntEle = Just 27.0, pntTime = Nothing}))
  , (GMAD, ICAOData (Point {pntLat = 30.333333333333332, pntLon = -9.4, pntEle = Just 74.0, pntTime = Nothing}))
  , (GMAT, ICAOData (Point {pntLat = 28.45, pntLon = -11.15, pntEle = Just 229.0, pntTime = Nothing}))
  , (GMFF, ICAOData (Point {pntLat = 33.93333333333333, pntLon = -4.983333333333333, pntEle = Just 579.0, pntTime = Nothing}))
  , (GMFI, ICAOData (Point {pntLat = 33.5, pntLon = -5.166666666666667, pntEle = Just 1664.0, pntTime = Nothing}))
  , (GMFK, ICAOData (Point {pntLat = 31.933333333333334, pntLon = -4.4, pntEle = Just 1034.0, pntTime = Nothing}))
  , (GMFM, ICAOData (Point {pntLat = 33.88333333333333, pntLon = -5.533333333333333, pntEle = Just 576.0, pntTime = Nothing}))
  , (GMFN, ICAOData (Point {pntLat = 35.15, pntLon = -2.9166666666666665, pntEle = Just 3.0, pntTime = Nothing}))
  , (GMFO, ICAOData (Point {pntLat = 34.78333333333333, pntLon = -1.9333333333333333, pntEle = Just 468.0, pntTime = Nothing}))
  , (GMFZ, ICAOData (Point {pntLat = 34.21666666666667, pntLon = -4.0, pntEle = Just 509.0, pntTime = Nothing}))
  , (GMMC, ICAOData (Point {pntLat = 33.56666666666667, pntLon = -7.666666666666667, pntEle = Just 62.0, pntTime = Nothing}))
  , (GMME, ICAOData (Point {pntLat = 34.05, pntLon = -6.766666666666667, pntEle = Just 84.0, pntTime = Nothing}))
  , (GMMF, ICAOData (Point {pntLat = 29.366666666666667, pntLon = -10.183333333333334, pntEle = Just 58.0, pntTime = Nothing}))
  , (GMMI, ICAOData (Point {pntLat = 31.516666666666666, pntLon = -9.783333333333333, pntEle = Just 15.0, pntTime = Nothing}))
  , (GMMN, ICAOData (Point {pntLat = 33.36666666666667, pntLon = -7.583333333333333, pntEle = Just 200.0, pntTime = Nothing}))
  , (GMMS, ICAOData (Point {pntLat = 32.28333333333333, pntLon = -9.233333333333333, pntEle = Just 52.0, pntTime = Nothing}))
  , (GMMX, ICAOData (Point {pntLat = 31.616666666666667, pntLon = -8.033333333333333, pntEle = Just 468.0, pntTime = Nothing}))
  , (GMMY, ICAOData (Point {pntLat = 34.3, pntLon = -6.6, pntEle = Just 5.0, pntTime = Nothing}))
  , (GMMZ, ICAOData (Point {pntLat = 30.933333333333334, pntLon = -6.9, pntEle = Just 1139.0, pntTime = Nothing}))
  , (GMTA, ICAOData (Point {pntLat = 35.18333333333333, pntLon = -3.85, pntEle = Just 27.0, pntTime = Nothing}))
  , (GMTN, ICAOData (Point {pntLat = 35.583333333333336, pntLon = -5.333333333333333, pntEle = Just 10.0, pntTime = Nothing}))
  , (GMTT, ICAOData (Point {pntLat = 35.733333333333334, pntLon = -5.9, pntEle = Just 19.0, pntTime = Nothing}))
  , (GOGG, ICAOData (Point {pntLat = 12.55, pntLon = -16.266666666666666, pntEle = Just 26.0, pntTime = Nothing}))
  , (GOGK, ICAOData (Point {pntLat = 12.883333333333333, pntLon = -14.966666666666667, pntEle = Just 10.0, pntTime = Nothing}))
  , (GOGS, ICAOData (Point {pntLat = 12.4, pntLon = -16.75, pntEle = Just 16.0, pntTime = Nothing}))
  , (GOOD, ICAOData (Point {pntLat = 14.65, pntLon = -16.233333333333334, pntEle = Just 7.0, pntTime = Nothing}))
  , (GOOG, ICAOData (Point {pntLat = 15.383333333333333, pntLon = -15.116666666666667, pntEle = Just 20.0, pntTime = Nothing}))
  , (GOOK, ICAOData (Point {pntLat = 14.133333333333333, pntLon = -16.066666666666666, pntEle = Just 6.0, pntTime = Nothing}))
  , (GOOY, ICAOData (Point {pntLat = 14.733333333333333, pntLon = -17.5, pntEle = Just 27.0, pntTime = Nothing}))
  , (GOSM, ICAOData (Point {pntLat = 15.65, pntLon = -13.25, pntEle = Just 15.0, pntTime = Nothing}))
  , (GOSP, ICAOData (Point {pntLat = 16.65, pntLon = -14.966666666666667, pntEle = Just 6.0, pntTime = Nothing}))
  , (GOSS, ICAOData (Point {pntLat = 16.05, pntLon = -16.45, pntEle = Just 4.0, pntTime = Nothing}))
  , (GOTK, ICAOData (Point {pntLat = 12.566666666666666, pntLon = -12.216666666666667, pntEle = Just 178.0, pntTime = Nothing}))
  , (GOTT, ICAOData (Point {pntLat = 13.766666666666667, pntLon = -13.683333333333334, pntEle = Just 49.0, pntTime = Nothing}))
  , (GQNA, ICAOData (Point {pntLat = 16.7, pntLon = -9.6, pntEle = Just 223.0, pntTime = Nothing}))
  , (GQNB, ICAOData (Point {pntLat = 17.533333333333335, pntLon = -14.683333333333334, pntEle = Just 77.0, pntTime = Nothing}))
  , (GQND, ICAOData (Point {pntLat = 18.566666666666666, pntLon = -11.433333333333334, pntEle = Just 396.0, pntTime = Nothing}))
  , (GQNF, ICAOData (Point {pntLat = 16.633333333333333, pntLon = -11.4, pntEle = Just 115.0, pntTime = Nothing}))
  , (GQNI, ICAOData (Point {pntLat = 16.6, pntLon = -7.266666666666667, pntEle = Just 269.0, pntTime = Nothing}))
  , (GQNJ, ICAOData (Point {pntLat = 19.75, pntLon = -14.366666666666667, pntEle = Just 123.0, pntTime = Nothing}))
  , (GQNK, ICAOData (Point {pntLat = 16.15, pntLon = -13.516666666666667, pntEle = Just 18.0, pntTime = Nothing}))
  , (GQNN, ICAOData (Point {pntLat = 18.1, pntLon = -15.95, pntEle = Just 2.0, pntTime = Nothing}))
  , (GQNR, ICAOData (Point {pntLat = 16.5, pntLon = -15.816666666666666, pntEle = Just 5.0, pntTime = Nothing}))
  , (GQPA, ICAOData (Point {pntLat = 20.516666666666666, pntLon = -13.066666666666666, pntEle = Just 226.0, pntTime = Nothing}))
  , (GQPP, ICAOData (Point {pntLat = 20.933333333333334, pntLon = -17.033333333333335, pntEle = Just 5.0, pntTime = Nothing}))
  , (GQPT, ICAOData (Point {pntLat = 25.233333333333334, pntLon = -11.616666666666667, pntEle = Just 364.0, pntTime = Nothing}))
  , (GSVO, ICAOData (Point {pntLat = 23.7, pntLon = -15.866666666666667, pntEle = Just 10.0, pntTime = Nothing}))
  , (GUCY, ICAOData (Point {pntLat = 9.566666666666666, pntLon = -13.616666666666667, pntEle = Just 26.0, pntTime = Nothing}))
  , (GUFH, ICAOData (Point {pntLat = 10.033333333333333, pntLon = -10.75, pntEle = Just 458.0, pntTime = Nothing}))
  , (GUID, ICAOData (Point {pntLat = 10.05, pntLon = -12.866666666666667, pntEle = Just 458.0, pntTime = Nothing}))
  , (GUKU, ICAOData (Point {pntLat = 9.183333333333334, pntLon = -10.1, pntEle = Just 525.0, pntTime = Nothing}))
  , (GULB, ICAOData (Point {pntLat = 11.316666666666666, pntLon = -12.3, pntEle = Just 1025.0, pntTime = Nothing}))
  , (GUMA, ICAOData (Point {pntLat = 8.533333333333333, pntLon = -9.466666666666667, pntEle = Just 543.0, pntTime = Nothing}))
  , (GUNZ, ICAOData (Point {pntLat = 7.733333333333333, pntLon = -8.833333333333334, pntEle = Just 470.0, pntTime = Nothing}))
  , (GUOK, ICAOData (Point {pntLat = 10.933333333333334, pntLon = -14.316666666666666, pntEle = Just 69.0, pntTime = Nothing}))
  , (GUSI, ICAOData (Point {pntLat = 11.433333333333334, pntLon = -9.166666666666666, pntEle = Just 362.0, pntTime = Nothing}))
  , (GUXD, ICAOData (Point {pntLat = 10.383333333333333, pntLon = -9.3, pntEle = Just 377.0, pntTime = Nothing}))
  , (GVAC, ICAOData (Point {pntLat = 16.733333333333334, pntLon = -22.95, pntEle = Just 54.0, pntTime = Nothing}))
  , (HAAB, ICAOData (Point {pntLat = 8.983333333333333, pntLon = 38.8, pntEle = Just 2355.0, pntTime = Nothing}))
  , (HAAM, ICAOData (Point {pntLat = 6.066666666666666, pntLon = 37.666666666666664, pntEle = Just 1219.0, pntTime = Nothing}))
  , (HABD, ICAOData (Point {pntLat = 11.6, pntLon = 37.4, pntEle = Just 1820.0, pntTime = Nothing}))
  , (HADC, ICAOData (Point {pntLat = 11.083333333333334, pntLon = 39.71666666666667, pntEle = Just 1864.0, pntTime = Nothing}))
  , (HADM, ICAOData (Point {pntLat = 10.35, pntLon = 37.71666666666667, pntEle = Just 2476.0, pntTime = Nothing}))
  , (HADR, ICAOData (Point {pntLat = 9.6, pntLon = 41.86666666666667, pntEle = Just 1146.0, pntTime = Nothing}))
  , (HAGN, ICAOData (Point {pntLat = 12.533333333333333, pntLon = 37.43333333333333, pntEle = Just 1985.0, pntTime = Nothing}))
  , (HAGO, ICAOData (Point {pntLat = 5.1, pntLon = 44.583333333333336, pntEle = Just 320.0, pntTime = Nothing}))
  , (HAGR, ICAOData (Point {pntLat = 8.166666666666666, pntLon = 35.55, pntEle = Just 1974.0, pntTime = Nothing}))
  , (HAHM, ICAOData (Point {pntLat = 8.733333333333333, pntLon = 39.0, pntEle = Just 1876.0, pntTime = Nothing}))
  , (HAJJ, ICAOData (Point {pntLat = 9.333333333333334, pntLon = 42.71666666666667, pntEle = Just 1644.0, pntTime = Nothing}))
  , (HAJM, ICAOData (Point {pntLat = 7.666666666666667, pntLon = 36.833333333333336, pntEle = Just 1676.0, pntTime = Nothing}))
  , (HALA, ICAOData (Point {pntLat = 7.066666666666666, pntLon = 38.5, pntEle = Just 1652.0, pntTime = Nothing}))
  , (HAMK, ICAOData (Point {pntLat = 13.5, pntLon = 39.483333333333334, pntEle = Just 2212.0, pntTime = Nothing}))
  , (HAMS, ICAOData (Point {pntLat = 15.616666666666667, pntLon = 39.45, pntEle = Just 10.0, pntTime = Nothing}))
  , (HANG, ICAOData (Point {pntLat = 5.283333333333333, pntLon = 39.75, pntEle = Just 1455.0, pntTime = Nothing}))
  , (HASB, ICAOData (Point {pntLat = 13.066666666666666, pntLon = 42.71666666666667, pntEle = Just 14.0, pntTime = Nothing}))
  , (HBBA, ICAOData (Point {pntLat = -3.3166666666666664, pntLon = 29.316666666666666, pntEle = Just 782.0, pntTime = Nothing}))
  , (HCMH, ICAOData (Point {pntLat = 9.5, pntLon = 44.083333333333336, pntEle = Just 1326.0, pntTime = Nothing}))
  , (HCMI, ICAOData (Point {pntLat = 10.416666666666666, pntLon = 45.016666666666666, pntEle = Just 9.0, pntTime = Nothing}))
  , (HCMM, ICAOData (Point {pntLat = 2.033333333333333, pntLon = 45.35, pntEle = Just 9.0, pntTime = Nothing}))
  , (HCMN, ICAOData (Point {pntLat = 4.7, pntLon = 45.21666666666667, pntEle = Just 173.0, pntTime = Nothing}))
  , (HCMV, ICAOData (Point {pntLat = 9.5, pntLon = 45.56666666666667, pntEle = Just 1032.0, pntTime = Nothing}))
  , (HEAR, ICAOData (Point {pntLat = 31.083333333333332, pntLon = 33.81666666666667, pntEle = Just 31.0, pntTime = Nothing}))
  , (HEAT, ICAOData (Point {pntLat = 27.05, pntLon = 31.016666666666666, pntEle = Just 226.0, pntTime = Nothing}))
  , (HEAX, ICAOData (Point {pntLat = 31.2, pntLon = 29.95, pntEle = Just (-2.0), pntTime = Nothing}))
  , (HECA, ICAOData (Point {pntLat = 30.133333333333333, pntLon = 31.4, pntEle = Just 64.0, pntTime = Nothing}))
  , (HEGN, ICAOData (Point {pntLat = 27.15, pntLon = 33.71666666666667, pntEle = Just 16.0, pntTime = Nothing}))
  , (HELX, ICAOData (Point {pntLat = 25.666666666666668, pntLon = 32.7, pntEle = Just 93.0, pntTime = Nothing}))
  , (HEMM, ICAOData (Point {pntLat = 31.333333333333332, pntLon = 27.216666666666665, pntEle = Just 25.0, pntTime = Nothing}))
  , (HEPS, ICAOData (Point {pntLat = 31.266666666666666, pntLon = 32.3, pntEle = Just 6.0, pntTime = Nothing}))
  , (HESN, ICAOData (Point {pntLat = 23.966666666666665, pntLon = 32.78333333333333, pntEle = Just 200.0, pntTime = Nothing}))
  , (HETR, ICAOData (Point {pntLat = 28.233333333333334, pntLon = 32.61666666666667, pntEle = Just 21.0, pntTime = Nothing}))
  , (HFFF, ICAOData (Point {pntLat = 11.55, pntLon = 43.15, pntEle = Just 13.0, pntTime = Nothing}))
  , (HHAS, ICAOData (Point {pntLat = 15.283333333333333, pntLon = 38.916666666666664, pntEle = Just 2356.0, pntTime = Nothing}))
  , (HKEL, ICAOData (Point {pntLat = 0.5333333333333333, pntLon = 35.28333333333333, pntEle = Just 2120.0, pntTime = Nothing}))
  , (HKEM, ICAOData (Point {pntLat = -0.5, pntLon = 37.45, pntEle = Just 1493.0, pntTime = Nothing}))
  , (HKGA, ICAOData (Point {pntLat = -0.4666666666666667, pntLon = 39.63333333333333, pntEle = Just 138.0, pntTime = Nothing}))
  , (HKKG, ICAOData (Point {pntLat = 0.2833333333333333, pntLon = 34.78333333333333, pntEle = Just 1530.0, pntTime = Nothing}))
  , (HKKI, ICAOData (Point {pntLat = -0.1, pntLon = 34.75, pntEle = Just 1157.0, pntTime = Nothing}))
  , (HKKR, ICAOData (Point {pntLat = -0.36666666666666664, pntLon = 35.35, pntEle = Just 2184.0, pntTime = Nothing}))
  , (HKKS, ICAOData (Point {pntLat = -0.6666666666666666, pntLon = 34.78333333333333, pntEle = Just 1493.0, pntTime = Nothing}))
  , (HKKT, ICAOData (Point {pntLat = 1.0166666666666666, pntLon = 35.0, pntEle = Just 1890.0, pntTime = Nothing}))
  , (HKLO, ICAOData (Point {pntLat = 3.1166666666666667, pntLon = 35.61666666666667, pntEle = Just 506.0, pntTime = Nothing}))
  , (HKLU, ICAOData (Point {pntLat = -2.2666666666666666, pntLon = 40.833333333333336, pntEle = Just 6.0, pntTime = Nothing}))
  , (HKMA, ICAOData (Point {pntLat = 3.9333333333333336, pntLon = 41.86666666666667, pntEle = Just 230.0, pntTime = Nothing}))
  , (HKMB, ICAOData (Point {pntLat = 2.3, pntLon = 37.9, pntEle = Just 1219.0, pntTime = Nothing}))
  , (HKME, ICAOData (Point {pntLat = 8.333333333333333e-2, pntLon = 37.65, pntEle = Just 1554.0, pntTime = Nothing}))
  , (HKML, ICAOData (Point {pntLat = -3.2333333333333334, pntLon = 40.1, pntEle = Just 20.0, pntTime = Nothing}))
  , (HKMO, ICAOData (Point {pntLat = -4.033333333333333, pntLon = 39.61666666666667, pntEle = Just 57.0, pntTime = Nothing}))
  , (HKMU, ICAOData (Point {pntLat = -2.283333333333333, pntLon = 37.833333333333336, pntEle = Just 1000.0, pntTime = Nothing}))
  , (HKMY, ICAOData (Point {pntLat = 3.533333333333333, pntLon = 39.05, pntEle = Just 1097.0, pntTime = Nothing}))
  , (HKNA, ICAOData (Point {pntLat = -1.3166666666666667, pntLon = 36.93333333333333, pntEle = Just 1624.0, pntTime = Nothing}))
  , (HKNC, ICAOData (Point {pntLat = -1.3, pntLon = 36.75, pntEle = Just 1798.0, pntTime = Nothing}))
  , (HKNI, ICAOData (Point {pntLat = -0.5, pntLon = 36.96666666666667, pntEle = Just 1759.0, pntTime = Nothing}))
  , (HKNK, ICAOData (Point {pntLat = -0.26666666666666666, pntLon = 36.1, pntEle = Just 1901.0, pntTime = Nothing}))
  , (HKNO, ICAOData (Point {pntLat = -1.1333333333333333, pntLon = 35.833333333333336, pntEle = Just 1890.0, pntTime = Nothing}))
  , (HKNW, ICAOData (Point {pntLat = -1.3166666666666667, pntLon = 36.81666666666667, pntEle = Just 1683.0, pntTime = Nothing}))
  , (HKVO, ICAOData (Point {pntLat = -3.4, pntLon = 38.56666666666667, pntEle = Just 579.0, pntTime = Nothing}))
  , (HKWJ, ICAOData (Point {pntLat = 1.75, pntLon = 40.06666666666667, pntEle = Just 244.0, pntTime = Nothing}))
  , (HLGT, ICAOData (Point {pntLat = 25.133333333333333, pntLon = 10.15, pntEle = Just 699.0, pntTime = Nothing}))
  , (HLKF, ICAOData (Point {pntLat = 24.216666666666665, pntLon = 23.3, pntEle = Just 417.0, pntTime = Nothing}))
  , (HLLB, ICAOData (Point {pntLat = 32.1, pntLon = 20.266666666666666, pntEle = Just 131.0, pntTime = Nothing}))
  , (HLLS, ICAOData (Point {pntLat = 27.016666666666666, pntLon = 14.45, pntEle = Just 435.0, pntTime = Nothing}))
  , (HLLT, ICAOData (Point {pntLat = 32.666666666666664, pntLon = 13.15, pntEle = Just 80.0, pntTime = Nothing}))
  , (HLTD, ICAOData (Point {pntLat = 30.133333333333333, pntLon = 9.5, pntEle = Just 346.0, pntTime = Nothing}))
  , (HRYG, ICAOData (Point {pntLat = -1.6666666666666665, pntLon = 29.25, pntEle = Just 1556.0, pntTime = Nothing}))
  , (HRYR, ICAOData (Point {pntLat = -1.9666666666666668, pntLon = 30.116666666666667, pntEle = Just 1491.0, pntTime = Nothing}))
  , (HSAT, ICAOData (Point {pntLat = 17.7, pntLon = 33.96666666666667, pntEle = Just 347.0, pntTime = Nothing}))
  , (HSDN, ICAOData (Point {pntLat = 19.166666666666668, pntLon = 30.483333333333334, pntEle = Just 226.0, pntTime = Nothing}))
  , (HSDZ, ICAOData (Point {pntLat = 11.783333333333333, pntLon = 34.38333333333333, pntEle = Just 474.0, pntTime = Nothing}))
  , (HSFS, ICAOData (Point {pntLat = 13.616666666666667, pntLon = 25.333333333333332, pntEle = Just 733.0, pntTime = Nothing}))
  , (HSGF, ICAOData (Point {pntLat = 14.033333333333333, pntLon = 35.4, pntEle = Just 599.0, pntTime = Nothing}))
  , (HSGN, ICAOData (Point {pntLat = 13.483333333333333, pntLon = 22.45, pntEle = Just 805.0, pntTime = Nothing}))
  , (HSKA, ICAOData (Point {pntLat = 15.466666666666667, pntLon = 36.4, pntEle = Just 500.0, pntTime = Nothing}))
  , (HSKI, ICAOData (Point {pntLat = 13.166666666666666, pntLon = 32.666666666666664, pntEle = Just 381.0, pntTime = Nothing}))
  , (HSLI, ICAOData (Point {pntLat = 11.0, pntLon = 29.716666666666665, pntEle = Just 499.0, pntTime = Nothing}))
  , (HSNL, ICAOData (Point {pntLat = 12.05, pntLon = 24.883333333333333, pntEle = Just 67.0, pntTime = Nothing}))
  , (HSNR, ICAOData (Point {pntLat = 13.55, pntLon = 33.61666666666667, pntEle = Just 418.0, pntTime = Nothing}))
  , (HSOB, ICAOData (Point {pntLat = 13.166666666666666, pntLon = 30.233333333333334, pntEle = Just 574.0, pntTime = Nothing}))
  , (HSRN, ICAOData (Point {pntLat = 11.75, pntLon = 32.78333333333333, pntEle = Just 282.0, pntTime = Nothing}))
  , (HSSJ, ICAOData (Point {pntLat = 4.866666666666667, pntLon = 31.6, pntEle = Just 460.0, pntTime = Nothing}))
  , (HSSM, ICAOData (Point {pntLat = 9.55, pntLon = 31.65, pntEle = Just 387.0, pntTime = Nothing}))
  , (HSSP, ICAOData (Point {pntLat = 19.583333333333332, pntLon = 37.21666666666667, pntEle = Just 3.0, pntTime = Nothing}))
  , (HSSS, ICAOData (Point {pntLat = 15.6, pntLon = 32.55, pntEle = Just 382.0, pntTime = Nothing}))
  , (HSSW, ICAOData (Point {pntLat = 21.816666666666666, pntLon = 31.483333333333334, pntEle = Just 183.0, pntTime = Nothing}))
  , (HTAR, ICAOData (Point {pntLat = -3.3333333333333335, pntLon = 36.61666666666667, pntEle = Just 1387.0, pntTime = Nothing}))
  , (HTBU, ICAOData (Point {pntLat = -1.3333333333333333, pntLon = 31.816666666666666, pntEle = Just 1137.0, pntTime = Nothing}))
  , (HTDA, ICAOData (Point {pntLat = -6.866666666666667, pntLon = 39.2, pntEle = Just 55.0, pntTime = Nothing}))
  , (HTDO, ICAOData (Point {pntLat = -6.166666666666667, pntLon = 35.766666666666666, pntEle = Just 1119.0, pntTime = Nothing}))
  , (HTIR, ICAOData (Point {pntLat = -7.666666666666667, pntLon = 35.75, pntEle = Just 1426.0, pntTime = Nothing}))
  , (HTKA, ICAOData (Point {pntLat = -4.883333333333333, pntLon = 29.633333333333333, pntEle = Just 882.0, pntTime = Nothing}))
  , (HTKJ, ICAOData (Point {pntLat = -3.4166666666666665, pntLon = 37.06666666666667, pntEle = Just 891.0, pntTime = Nothing}))
  , (HTMB, ICAOData (Point {pntLat = -8.933333333333334, pntLon = 33.46666666666667, pntEle = Just 1704.0, pntTime = Nothing}))
  , (HTMG, ICAOData (Point {pntLat = -6.833333333333333, pntLon = 37.65, pntEle = Just 526.0, pntTime = Nothing}))
  , (HTMO, ICAOData (Point {pntLat = -4.883333333333333, pntLon = 38.28333333333333, pntEle = Just 511.0, pntTime = Nothing}))
  , (HTMS, ICAOData (Point {pntLat = -3.35, pntLon = 37.333333333333336, pntEle = Just 854.0, pntTime = Nothing}))
  , (HTMT, ICAOData (Point {pntLat = -10.266666666666667, pntLon = 40.18333333333333, pntEle = Just 113.0, pntTime = Nothing}))
  , (HTMU, ICAOData (Point {pntLat = -1.5, pntLon = 33.8, pntEle = Just 1147.0, pntTime = Nothing}))
  , (HTMW, ICAOData (Point {pntLat = -2.466666666666667, pntLon = 32.916666666666664, pntEle = Just 1139.0, pntTime = Nothing}))
  , (HTNA, ICAOData (Point {pntLat = -10.35, pntLon = 38.75, pntEle = Just 463.0, pntTime = Nothing}))
  , (HTPE, ICAOData (Point {pntLat = -5.25, pntLon = 39.81666666666667, pntEle = Just 25.0, pntTime = Nothing}))
  , (HTSE, ICAOData (Point {pntLat = -4.083333333333333, pntLon = 37.71666666666667, pntEle = Just 872.0, pntTime = Nothing}))
  , (HTSO, ICAOData (Point {pntLat = -10.683333333333334, pntLon = 35.583333333333336, pntEle = Just 1067.0, pntTime = Nothing}))
  , (HTTB, ICAOData (Point {pntLat = -5.083333333333333, pntLon = 32.833333333333336, pntEle = Just 1181.0, pntTime = Nothing}))
  , (HTTG, ICAOData (Point {pntLat = -5.083333333333333, pntLon = 39.06666666666667, pntEle = Just 39.0, pntTime = Nothing}))
  , (HTZA, ICAOData (Point {pntLat = -6.216666666666667, pntLon = 39.21666666666667, pntEle = Just 15.0, pntTime = Nothing}))
  , (HUAR, ICAOData (Point {pntLat = 3.05, pntLon = 30.916666666666668, pntEle = Just 1204.0, pntTime = Nothing}))
  , (HUEN, ICAOData (Point {pntLat = 5.0e-2, pntLon = 32.45, pntEle = Just 1155.0, pntTime = Nothing}))
  , (HUGU, ICAOData (Point {pntLat = 2.75, pntLon = 32.333333333333336, pntEle = Just 1104.0, pntTime = Nothing}))
  , (HUJI, ICAOData (Point {pntLat = 0.45, pntLon = 33.18333333333333, pntEle = Just 1175.0, pntTime = Nothing}))
  , (HUKB, ICAOData (Point {pntLat = -1.25, pntLon = 29.983333333333334, pntEle = Just 1867.0, pntTime = Nothing}))
  , (HUKS, ICAOData (Point {pntLat = 0.18333333333333332, pntLon = 30.1, pntEle = Just 959.0, pntTime = Nothing}))
  , (HUMA, ICAOData (Point {pntLat = -0.6166666666666667, pntLon = 30.65, pntEle = Just 1412.0, pntTime = Nothing}))
  , (HUMI, ICAOData (Point {pntLat = 1.6833333333333333, pntLon = 31.716666666666665, pntEle = Just 1146.0, pntTime = Nothing}))
  , (HUSO, ICAOData (Point {pntLat = 1.7166666666666668, pntLon = 33.61666666666667, pntEle = Just 1132.0, pntTime = Nothing}))
  , (HUTO, ICAOData (Point {pntLat = 0.6833333333333333, pntLon = 34.166666666666664, pntEle = Just 1170.0, pntTime = Nothing}))
  , (K01R, ICAOData (Point {pntLat = 31.133333333333333, pntLon = -92.56666666666666, pntEle = Just 67.0, pntTime = Nothing}))
  , (K1K5, ICAOData (Point {pntLat = 37.0, pntLon = -101.88333333333334, pntEle = Just 1102.0, pntTime = Nothing}))
  , (K1V4, ICAOData (Point {pntLat = 44.416666666666664, pntLon = -72.01666666666667, pntEle = Just 212.0, pntTime = Nothing}))
  , (K2C2, ICAOData (Point {pntLat = 32.38333333333333, pntLon = -106.48333333333333, pntEle = Just 1244.0, pntTime = Nothing}))
  , (K2DP, ICAOData (Point {pntLat = 35.666666666666664, pntLon = -75.9, pntEle = Just 3.0, pntTime = Nothing}))
  , (K2PJ, ICAOData (Point {pntLat = 33.85, pntLon = -80.48333333333333, pntEle = Just 68.0, pntTime = Nothing}))
  , (K3RN, ICAOData (Point {pntLat = 44.833333333333336, pntLon = -84.55, pntEle = Just 387.0, pntTime = Nothing}))
  , (K44W, ICAOData (Point {pntLat = 35.15, pntLon = -75.3, pntEle = Just 1.0, pntTime = Nothing}))
  , (K4CB, ICAOData (Point {pntLat = 35.266666666666666, pntLon = -117.43333333333334, pntEle = Just 864.0, pntTime = Nothing}))
  , (K4MR, ICAOData (Point {pntLat = 34.3, pntLon = -103.8, pntEle = Just 1326.0, pntTime = Nothing}))
  , (K4SU, ICAOData (Point {pntLat = 36.333333333333336, pntLon = -117.1, pntEle = Just 962.0, pntTime = Nothing}))
  , (K50Q, ICAOData (Point {pntLat = 37.7, pntLon = -123.0, pntEle = Just 12.0, pntTime = Nothing}))
  , (K87Q, ICAOData (Point {pntLat = 35.65, pntLon = -121.28333333333333, pntEle = Just 17.0, pntTime = Nothing}))
  , (K9B2, ICAOData (Point {pntLat = 44.93333333333333, pntLon = -72.2, pntEle = Just 233.0, pntTime = Nothing}))
  , (K9V9, ICAOData (Point {pntLat = 43.766666666666666, pntLon = -99.31666666666666, pntEle = Just 517.0, pntTime = Nothing}))
  , (KABE, ICAOData (Point {pntLat = 40.65, pntLon = -75.45, pntEle = Just 120.0, pntTime = Nothing}))
  , (KABI, ICAOData (Point {pntLat = 32.416666666666664, pntLon = -99.68333333333334, pntEle = Just 546.0, pntTime = Nothing}))
  , (KABQ, ICAOData (Point {pntLat = 35.03333333333333, pntLon = -106.6, pntEle = Just 1631.0, pntTime = Nothing}))
  , (KABR, ICAOData (Point {pntLat = 45.45, pntLon = -98.41666666666667, pntEle = Just 397.0, pntTime = Nothing}))
  , (KABY, ICAOData (Point {pntLat = 31.533333333333335, pntLon = -84.2, pntEle = Just 60.0, pntTime = Nothing}))
  , (KACT, ICAOData (Point {pntLat = 31.6, pntLon = -97.21666666666667, pntEle = Just 157.0, pntTime = Nothing}))
  , (KACY, ICAOData (Point {pntLat = 39.46666666666667, pntLon = -74.58333333333333, pntEle = Just 23.0, pntTime = Nothing}))
  , (KADW, ICAOData (Point {pntLat = 38.81666666666667, pntLon = -76.85, pntEle = Just 86.0, pntTime = Nothing}))
  , (KAEX, ICAOData (Point {pntLat = 31.55, pntLon = -92.93333333333334, pntEle = Just 27.0, pntTime = Nothing}))
  , (KAFF, ICAOData (Point {pntLat = 38.96666666666667, pntLon = -104.81666666666666, pntEle = Just 2003.0, pntTime = Nothing}))
  , (KAGR, ICAOData (Point {pntLat = 27.65, pntLon = -81.33333333333333, pntEle = Just 20.0, pntTime = Nothing}))
  , (KAGS, ICAOData (Point {pntLat = 33.36666666666667, pntLon = -81.96666666666667, pntEle = Just 44.0, pntTime = Nothing}))
  , (KAHN, ICAOData (Point {pntLat = 33.95, pntLon = -83.33333333333333, pntEle = Just 246.0, pntTime = Nothing}))
  , (KALB, ICAOData (Point {pntLat = 42.75, pntLon = -73.8, pntEle = Just 87.0, pntTime = Nothing}))
  , (KALO, ICAOData (Point {pntLat = 42.55, pntLon = -92.4, pntEle = Just 266.0, pntTime = Nothing}))
  , (KALS, ICAOData (Point {pntLat = 37.43333333333333, pntLon = -105.86666666666666, pntEle = Just 2297.0, pntTime = Nothing}))
  , (KAMA, ICAOData (Point {pntLat = 35.21666666666667, pntLon = -101.71666666666667, pntEle = Just 1099.0, pntTime = Nothing}))
  , (KANJ, ICAOData (Point {pntLat = 46.483333333333334, pntLon = -84.35, pntEle = Just 218.0, pntTime = Nothing}))
  , (KAPN, ICAOData (Point {pntLat = 45.083333333333336, pntLon = -83.56666666666666, pntEle = Just 210.0, pntTime = Nothing}))
  , (KAQQ, ICAOData (Point {pntLat = 29.716666666666665, pntLon = -85.01666666666667, pntEle = Just 6.0, pntTime = Nothing}))
  , (KAST, ICAOData (Point {pntLat = 46.15, pntLon = -123.88333333333334, pntEle = Just 3.0, pntTime = Nothing}))
  , (KATL, ICAOData (Point {pntLat = 33.65, pntLon = -84.43333333333334, pntEle = Just 313.0, pntTime = Nothing}))
  , (KATT, ICAOData (Point {pntLat = 30.316666666666666, pntLon = -97.76666666666667, pntEle = Just 201.0, pntTime = Nothing}))
  , (KAVL, ICAOData (Point {pntLat = 35.43333333333333, pntLon = -82.53333333333333, pntEle = Just 660.0, pntTime = Nothing}))
  , (KAVP, ICAOData (Point {pntLat = 41.333333333333336, pntLon = -75.73333333333333, pntEle = Just 293.0, pntTime = Nothing}))
  , (KAVX, ICAOData (Point {pntLat = 33.4, pntLon = -118.41666666666667, pntEle = Just 488.0, pntTime = Nothing}))
  , (KAYS, ICAOData (Point {pntLat = 31.25, pntLon = -82.4, pntEle = Just 46.0, pntTime = Nothing}))
  , (KBDL, ICAOData (Point {pntLat = 41.93333333333333, pntLon = -72.68333333333334, pntEle = Just 53.0, pntTime = Nothing}))
  , (KBDR, ICAOData (Point {pntLat = 41.166666666666664, pntLon = -73.13333333333334, pntEle = Just 3.0, pntTime = Nothing}))
  , (KBED, ICAOData (Point {pntLat = 42.46666666666667, pntLon = -71.3, pntEle = Just 41.0, pntTime = Nothing}))
  , (KBFF, ICAOData (Point {pntLat = 41.86666666666667, pntLon = -103.6, pntEle = Just 1209.0, pntTime = Nothing}))
  , (KBFL, ICAOData (Point {pntLat = 35.43333333333333, pntLon = -119.05, pntEle = Just 155.0, pntTime = Nothing}))
  , (KBGM, ICAOData (Point {pntLat = 42.2, pntLon = -75.98333333333333, pntEle = Just 499.0, pntTime = Nothing}))
  , (KBHM, ICAOData (Point {pntLat = 33.56666666666667, pntLon = -86.75, pntEle = Just 196.0, pntTime = Nothing}))
  , (KBIH, ICAOData (Point {pntLat = 37.36666666666667, pntLon = -118.36666666666666, pntEle = Just 1256.0, pntTime = Nothing}))
  , (KBIL, ICAOData (Point {pntLat = 45.81666666666667, pntLon = -108.55, pntEle = Just 1112.0, pntTime = Nothing}))
  , (KBIS, ICAOData (Point {pntLat = 46.78333333333333, pntLon = -100.75, pntEle = Just 511.0, pntTime = Nothing}))
  , (KBJI, ICAOData (Point {pntLat = 47.5, pntLon = -94.93333333333334, pntEle = Just 423.0, pntTime = Nothing}))
  , (KBJN, ICAOData (Point {pntLat = 37.61666666666667, pntLon = -116.25, pntEle = Just 1756.0, pntTime = Nothing}))
  , (KBKW, ICAOData (Point {pntLat = 37.8, pntLon = -81.11666666666666, pntEle = Just 763.0, pntTime = Nothing}))
  , (KBML, ICAOData (Point {pntLat = 44.583333333333336, pntLon = -71.18333333333334, pntEle = Just 353.0, pntTime = Nothing}))
  , (KBNA, ICAOData (Point {pntLat = 36.11666666666667, pntLon = -86.68333333333334, pntEle = Just 183.0, pntTime = Nothing}))
  , (KBNO, ICAOData (Point {pntLat = 43.6, pntLon = -118.95, pntEle = Just 1263.0, pntTime = Nothing}))
  , (KBOI, ICAOData (Point {pntLat = 43.56666666666667, pntLon = -116.23333333333333, pntEle = Just 871.0, pntTime = Nothing}))
  , (KBOS, ICAOData (Point {pntLat = 42.36666666666667, pntLon = -71.01666666666667, pntEle = Just 6.0, pntTime = Nothing}))
  , (KBPI, ICAOData (Point {pntLat = 42.583333333333336, pntLon = -110.1, pntEle = Just 2131.0, pntTime = Nothing}))
  , (KBPT, ICAOData (Point {pntLat = 30.583333333333332, pntLon = -94.13333333333334, pntEle = Just 5.0, pntTime = Nothing}))
  , (KBRO, ICAOData (Point {pntLat = 25.9, pntLon = -97.41666666666667, pntEle = Just 7.0, pntTime = Nothing}))
  , (KBTV, ICAOData (Point {pntLat = 44.46666666666667, pntLon = -73.15, pntEle = Just 102.0, pntTime = Nothing}))
  , (KBUF, ICAOData (Point {pntLat = 42.93333333333333, pntLon = -78.73333333333333, pntEle = Just 221.0, pntTime = Nothing}))
  , (KBUR, ICAOData (Point {pntLat = 34.2, pntLon = -118.36666666666666, pntEle = Just 236.0, pntTime = Nothing}))
  , (KBVE, ICAOData (Point {pntLat = 29.333333333333332, pntLon = -89.4, pntEle = Just 0.0, pntTime = Nothing}))
  , (KBWI, ICAOData (Point {pntLat = 39.166666666666664, pntLon = -76.68333333333334, pntEle = Just 45.0, pntTime = Nothing}))
  , (KBYS, ICAOData (Point {pntLat = 35.28333333333333, pntLon = -116.61666666666666, pntEle = Just 716.0, pntTime = Nothing}))
  , (KCAE, ICAOData (Point {pntLat = 33.95, pntLon = -81.11666666666666, pntEle = Just 72.0, pntTime = Nothing}))
  , (KCAG, ICAOData (Point {pntLat = 40.5, pntLon = -107.51666666666667, pntEle = Just 1888.0, pntTime = Nothing}))
  , (KCAK, ICAOData (Point {pntLat = 40.916666666666664, pntLon = -81.45, pntEle = Just 374.0, pntTime = Nothing}))
  , (KCAO, ICAOData (Point {pntLat = 36.45, pntLon = -103.15, pntEle = Just 1513.0, pntTime = Nothing}))
  , (KCAR, ICAOData (Point {pntLat = 46.86666666666667, pntLon = -68.01666666666667, pntEle = Just 191.0, pntTime = Nothing}))
  , (KCEF, ICAOData (Point {pntLat = 42.2, pntLon = -72.53333333333333, pntEle = Just 75.0, pntTime = Nothing}))
  , (KCHA, ICAOData (Point {pntLat = 35.03333333333333, pntLon = -85.2, pntEle = Just 208.0, pntTime = Nothing}))
  , (KCHH, ICAOData (Point {pntLat = 41.666666666666664, pntLon = -69.96666666666667, pntEle = Just 14.0, pntTime = Nothing}))
  , (KCHS, ICAOData (Point {pntLat = 32.9, pntLon = -80.03333333333333, pntEle = Just 14.0, pntTime = Nothing}))
  , (KCKL, ICAOData (Point {pntLat = 32.9, pntLon = -87.25, pntEle = Just 140.0, pntTime = Nothing}))
  , (KCLE, ICAOData (Point {pntLat = 41.4, pntLon = -81.85, pntEle = Just 241.0, pntTime = Nothing}))
  , (KCLT, ICAOData (Point {pntLat = 35.21666666666667, pntLon = -80.95, pntEle = Just 228.0, pntTime = Nothing}))
  , (KCMH, ICAOData (Point {pntLat = 40.0, pntLon = -82.88333333333334, pntEle = Just 248.0, pntTime = Nothing}))
  , (KCMX, ICAOData (Point {pntLat = 47.166666666666664, pntLon = -88.48333333333333, pntEle = Just 334.0, pntTime = Nothing}))
  , (KCNK, ICAOData (Point {pntLat = 39.55, pntLon = -97.65, pntEle = Just 453.0, pntTime = Nothing}))
  , (KCOD, ICAOData (Point {pntLat = 44.516666666666666, pntLon = -109.01666666666667, pntEle = Just 1551.0, pntTime = Nothing}))
  , (KCOF, ICAOData (Point {pntLat = 28.233333333333334, pntLon = -80.6, pntEle = Just 3.0, pntTime = Nothing}))
  , (KCON, ICAOData (Point {pntLat = 43.18333333333333, pntLon = -71.5, pntEle = Just 105.0, pntTime = Nothing}))
  , (KCOS, ICAOData (Point {pntLat = 38.81666666666667, pntLon = -104.71666666666667, pntEle = Just 1885.0, pntTime = Nothing}))
  , (KCOU, ICAOData (Point {pntLat = 38.81666666666667, pntLon = -92.21666666666667, pntEle = Just 271.0, pntTime = Nothing}))
  , (KCPR, ICAOData (Point {pntLat = 42.9, pntLon = -106.46666666666667, pntEle = Just 1630.0, pntTime = Nothing}))
  , (KCRP, ICAOData (Point {pntLat = 27.766666666666666, pntLon = -97.51666666666667, pntEle = Just 13.0, pntTime = Nothing}))
  , (KCRW, ICAOData (Point {pntLat = 38.36666666666667, pntLon = -81.6, pntEle = Just 299.0, pntTime = Nothing}))
  , (KCTY, ICAOData (Point {pntLat = 29.633333333333333, pntLon = -83.1, pntEle = Just 13.0, pntTime = Nothing}))
  , (KCVG, ICAOData (Point {pntLat = 39.05, pntLon = -84.66666666666667, pntEle = Just 273.0, pntTime = Nothing}))
  , (KCYS, ICAOData (Point {pntLat = 41.15, pntLon = -104.8, pntEle = Just 1878.0, pntTime = Nothing}))
  , (KD45, ICAOData (Point {pntLat = 48.93333333333333, pntLon = -95.35, pntEle = Just 328.0, pntTime = Nothing}))
  , (KDAY, ICAOData (Point {pntLat = 39.9, pntLon = -84.21666666666667, pntEle = Just 308.0, pntTime = Nothing}))
  , (KDBQ, ICAOData (Point {pntLat = 42.4, pntLon = -90.7, pntEle = Just 328.0, pntTime = Nothing}))
  , (KDCA, ICAOData (Point {pntLat = 38.85, pntLon = -77.03333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (KDDC, ICAOData (Point {pntLat = 37.766666666666666, pntLon = -99.96666666666667, pntEle = Just 791.0, pntTime = Nothing}))
  , (KDEN, ICAOData (Point {pntLat = 39.86666666666667, pntLon = -104.66666666666667, pntEle = Just 1655.0, pntTime = Nothing}))
  , (KDFW, ICAOData (Point {pntLat = 32.9, pntLon = -97.03333333333333, pntEle = Just 184.0, pntTime = Nothing}))
  , (KDGW, ICAOData (Point {pntLat = 42.8, pntLon = -105.38333333333334, pntEle = Just 1502.0, pntTime = Nothing}))
  , (KDLH, ICAOData (Point {pntLat = 46.833333333333336, pntLon = -92.21666666666667, pntEle = Just 435.0, pntTime = Nothing}))
  , (KDNR, ICAOData (Point {pntLat = 39.78333333333333, pntLon = -104.86666666666666, pntEle = Just 1626.0, pntTime = Nothing}))
  , (KDPG, ICAOData (Point {pntLat = 40.166666666666664, pntLon = -112.93333333333334, pntEle = Just 1326.0, pntTime = Nothing}))
  , (KDRA, ICAOData (Point {pntLat = 36.63333333333333, pntLon = -116.03333333333333, pntEle = Just 1010.0, pntTime = Nothing}))
  , (KDRT, ICAOData (Point {pntLat = 29.366666666666667, pntLon = -100.91666666666667, pntEle = Just 304.0, pntTime = Nothing}))
  , (KDSM, ICAOData (Point {pntLat = 41.53333333333333, pntLon = -93.66666666666667, pntEle = Just 292.0, pntTime = Nothing}))
  , (KDTW, ICAOData (Point {pntLat = 42.233333333333334, pntLon = -83.33333333333333, pntEle = Just 195.0, pntTime = Nothing}))
  , (KE28, ICAOData (Point {pntLat = 33.9, pntLon = -106.4, pntEle = Just 1193.0, pntTime = Nothing}))
  , (KEDW, ICAOData (Point {pntLat = 34.916666666666664, pntLon = -117.9, pntEle = Just 702.0, pntTime = Nothing}))
  , (KEKA, ICAOData (Point {pntLat = 40.8, pntLon = -124.16666666666667, pntEle = Just 13.0, pntTime = Nothing}))
  , (KEKN, ICAOData (Point {pntLat = 38.88333333333333, pntLon = -79.85, pntEle = Just 606.0, pntTime = Nothing}))
  , (KELP, ICAOData (Point {pntLat = 31.816666666666666, pntLon = -106.38333333333334, pntEle = Just 1206.0, pntTime = Nothing}))
  , (KELY, ICAOData (Point {pntLat = 39.3, pntLon = -114.85, pntEle = Just 1907.0, pntTime = Nothing}))
  , (KENV, ICAOData (Point {pntLat = 40.733333333333334, pntLon = -114.03333333333333, pntEle = Just 1291.0, pntTime = Nothing}))
  , (KEPO, ICAOData (Point {pntLat = 44.916666666666664, pntLon = -67.0, pntEle = Just 24.0, pntTime = Nothing}))
  , (KEPZ, ICAOData (Point {pntLat = 31.866666666666667, pntLon = -106.7, pntEle = Just 1252.0, pntTime = Nothing}))
  , (KERI, ICAOData (Point {pntLat = 42.083333333333336, pntLon = -80.18333333333334, pntEle = Just 223.0, pntTime = Nothing}))
  , (KESC, ICAOData (Point {pntLat = 45.75, pntLon = -87.03333333333333, pntEle = Just 180.0, pntTime = Nothing}))
  , (KEUG, ICAOData (Point {pntLat = 44.13333333333333, pntLon = -123.21666666666667, pntEle = Just 111.0, pntTime = Nothing}))
  , (KEVV, ICAOData (Point {pntLat = 38.03333333333333, pntLon = -87.53333333333333, pntEle = Just 127.0, pntTime = Nothing}))
  , (KEWR, ICAOData (Point {pntLat = 40.68333333333333, pntLon = -74.16666666666667, pntEle = Just 5.0, pntTime = Nothing}))
  , (KEYW, ICAOData (Point {pntLat = 24.55, pntLon = -81.76666666666667, pntEle = Just 1.0, pntTime = Nothing}))
  , (KF10, ICAOData (Point {pntLat = 35.4, pntLon = -96.01666666666667, pntEle = Just 259.0, pntTime = Nothing}))
  , (KF30, ICAOData (Point {pntLat = 34.516666666666666, pntLon = -96.98333333333333, pntEle = Just 320.0, pntTime = Nothing}))
  , (KFAR, ICAOData (Point {pntLat = 46.93333333333333, pntLon = -96.81666666666666, pntEle = Just 274.0, pntTime = Nothing}))
  , (KFAT, ICAOData (Point {pntLat = 36.78333333333333, pntLon = -119.71666666666667, pntEle = Just 101.0, pntTime = Nothing}))
  , (KFBG, ICAOData (Point {pntLat = 35.13333333333333, pntLon = -78.93333333333334, pntEle = Just 74.0, pntTime = Nothing}))
  , (KFCS, ICAOData (Point {pntLat = 38.7, pntLon = -104.76666666666667, pntEle = Just 1789.0, pntTime = Nothing}))
  , (KFFO, ICAOData (Point {pntLat = 39.833333333333336, pntLon = -84.05, pntEle = Just 251.0, pntTime = Nothing}))
  , (KFHU, ICAOData (Point {pntLat = 47.63333333333333, pntLon = -110.33333333333333, pntEle = Just 1438.0, pntTime = Nothing}))
  , (KFMH, ICAOData (Point {pntLat = 41.65, pntLon = -70.51666666666667, pntEle = Just 40.0, pntTime = Nothing}))
  , (KFNT, ICAOData (Point {pntLat = 42.96666666666667, pntLon = -83.75, pntEle = Just 238.0, pntTime = Nothing}))
  , (KFOD, ICAOData (Point {pntLat = 42.55, pntLon = -94.18333333333334, pntEle = Just 354.0, pntTime = Nothing}))
  , (KFRI, ICAOData (Point {pntLat = 39.05, pntLon = -96.75, pntEle = Just 324.0, pntTime = Nothing}))
  , (KFSD, ICAOData (Point {pntLat = 43.583333333333336, pntLon = -96.75, pntEle = Just 436.0, pntTime = Nothing}))
  , (KFSI, ICAOData (Point {pntLat = 34.6, pntLon = -98.4, pntEle = Just 362.0, pntTime = Nothing}))
  , (KFSM, ICAOData (Point {pntLat = 35.333333333333336, pntLon = -94.36666666666666, pntEle = Just 143.0, pntTime = Nothing}))
  , (KFTK, ICAOData (Point {pntLat = 37.9, pntLon = -85.96666666666667, pntEle = Just 230.0, pntTime = Nothing}))
  , (KFWA, ICAOData (Point {pntLat = 41.0, pntLon = -85.2, pntEle = Just 248.0, pntTime = Nothing}))
  , (KFWD, ICAOData (Point {pntLat = 32.833333333333336, pntLon = -97.3, pntEle = Just 196.0, pntTime = Nothing}))
  , (KGBN, ICAOData (Point {pntLat = 32.43333333333333, pntLon = -112.68333333333334, pntEle = Just 262.0, pntTime = Nothing}))
  , (KGCC, ICAOData (Point {pntLat = 44.333333333333336, pntLon = -105.55, pntEle = Just 1330.0, pntTime = Nothing}))
  , (KGDP, ICAOData (Point {pntLat = 31.833333333333332, pntLon = -104.81666666666666, pntEle = Just 1663.0, pntTime = Nothing}))
  , (KGEG, ICAOData (Point {pntLat = 47.61666666666667, pntLon = -117.53333333333333, pntEle = Just 723.0, pntTime = Nothing}))
  , (KGGG, ICAOData (Point {pntLat = 32.38333333333333, pntLon = -94.71666666666667, pntEle = Just 111.0, pntTime = Nothing}))
  , (KGGW, ICAOData (Point {pntLat = 48.21666666666667, pntLon = -106.61666666666666, pntEle = Just 699.0, pntTime = Nothing}))
  , (KGJT, ICAOData (Point {pntLat = 39.13333333333333, pntLon = -108.53333333333333, pntEle = Just 1481.0, pntTime = Nothing}))
  , (KGLD, ICAOData (Point {pntLat = 39.36666666666667, pntLon = -101.7, pntEle = Just 1114.0, pntTime = Nothing}))
  , (KGLS, ICAOData (Point {pntLat = 29.266666666666666, pntLon = -94.86666666666666, pntEle = Just 2.0, pntTime = Nothing}))
  , (KGPI, ICAOData (Point {pntLat = 48.3, pntLon = -114.26666666666667, pntEle = Just 906.0, pntTime = Nothing}))
  , (KGRB, ICAOData (Point {pntLat = 44.483333333333334, pntLon = -88.13333333333334, pntEle = Just 212.0, pntTime = Nothing}))
  , (KGRF, ICAOData (Point {pntLat = 47.11666666666667, pntLon = -122.55, pntEle = Just 92.0, pntTime = Nothing}))
  , (KGRI, ICAOData (Point {pntLat = 40.96666666666667, pntLon = -98.31666666666666, pntEle = Just 563.0, pntTime = Nothing}))
  , (KGRR, ICAOData (Point {pntLat = 42.88333333333333, pntLon = -85.51666666666667, pntEle = Just 242.0, pntTime = Nothing}))
  , (KGSO, ICAOData (Point {pntLat = 36.1, pntLon = -79.95, pntEle = Just 282.0, pntTime = Nothing}))
  , (KGSP, ICAOData (Point {pntLat = 34.88333333333333, pntLon = -82.21666666666667, pntEle = Just 296.0, pntTime = Nothing}))
  , (KGTB, ICAOData (Point {pntLat = 44.05, pntLon = -75.73333333333333, pntEle = Just 207.0, pntTime = Nothing}))
  , (KGTF, ICAOData (Point {pntLat = 47.46666666666667, pntLon = -111.38333333333334, pntEle = Just 1120.0, pntTime = Nothing}))
  , (KH92, ICAOData (Point {pntLat = 36.43333333333333, pntLon = -96.38333333333334, pntEle = Just 251.0, pntTime = Nothing}))
  , (KHAT, ICAOData (Point {pntLat = 35.266666666666666, pntLon = -75.55, pntEle = Just 2.0, pntTime = Nothing}))
  , (KHLN, ICAOData (Point {pntLat = 46.6, pntLon = -111.96666666666667, pntEle = Just 1180.0, pntTime = Nothing}))
  , (KHLR, ICAOData (Point {pntLat = 31.133333333333333, pntLon = -97.7, pntEle = Just 282.0, pntTime = Nothing}))
  , (KHMN, ICAOData (Point {pntLat = 32.85, pntLon = -106.1, pntEle = Just 1248.0, pntTime = Nothing}))
  , (KHMS, ICAOData (Point {pntLat = 46.56666666666667, pntLon = -119.6, pntEle = Just 223.0, pntTime = Nothing}))
  , (KHON, ICAOData (Point {pntLat = 44.38333333333333, pntLon = -98.23333333333333, pntEle = Just 393.0, pntTime = Nothing}))
  , (KHOP, ICAOData (Point {pntLat = 36.666666666666664, pntLon = -87.5, pntEle = Just 174.0, pntTime = Nothing}))
  , (KHRT, ICAOData (Point {pntLat = 30.416666666666668, pntLon = -86.68333333333334, pntEle = Just 12.0, pntTime = Nothing}))
  , (KHSV, ICAOData (Point {pntLat = 34.65, pntLon = -86.78333333333333, pntEle = Just 192.0, pntTime = Nothing}))
  , (KHTL, ICAOData (Point {pntLat = 44.35, pntLon = -84.66666666666667, pntEle = Just 351.0, pntTime = Nothing}))
  , (KHTS, ICAOData (Point {pntLat = 38.36666666666667, pntLon = -82.55, pntEle = Just 252.0, pntTime = Nothing}))
  , (KHVR, ICAOData (Point {pntLat = 48.55, pntLon = -109.76666666666667, pntEle = Just 789.0, pntTime = Nothing}))
  , (KIAD, ICAOData (Point {pntLat = 38.93333333333333, pntLon = -77.45, pntEle = Just 95.0, pntTime = Nothing}))
  , (KIAH, ICAOData (Point {pntLat = 30.0, pntLon = -95.36666666666666, pntEle = Just 30.0, pntTime = Nothing}))
  , (KICT, ICAOData (Point {pntLat = 37.65, pntLon = -97.43333333333334, pntEle = Just 406.0, pntTime = Nothing}))
  , (KIGM, ICAOData (Point {pntLat = 35.25, pntLon = -113.93333333333334, pntEle = Just 1050.0, pntTime = Nothing}))
  , (KIND, ICAOData (Point {pntLat = 39.71666666666667, pntLon = -86.28333333333333, pntEle = Just 243.0, pntTime = Nothing}))
  , (KINL, ICAOData (Point {pntLat = 48.56666666666667, pntLon = -93.4, pntEle = Just 361.0, pntTime = Nothing}))
  , (KINS, ICAOData (Point {pntLat = 36.583333333333336, pntLon = -115.66666666666667, pntEle = Just 955.0, pntTime = Nothing}))
  , (KINW, ICAOData (Point {pntLat = 35.03333333333333, pntLon = -110.71666666666667, pntEle = Just 1505.0, pntTime = Nothing}))
  , (KIPT, ICAOData (Point {pntLat = 41.25, pntLon = -76.91666666666667, pntEle = Just 161.0, pntTime = Nothing}))
  , (KISN, ICAOData (Point {pntLat = 48.18333333333333, pntLon = -103.65, pntEle = Just 604.0, pntTime = Nothing}))
  , (KJAN, ICAOData (Point {pntLat = 32.31666666666667, pntLon = -90.06666666666666, pntEle = Just 105.0, pntTime = Nothing}))
  , (KJAX, ICAOData (Point {pntLat = 30.5, pntLon = -81.7, pntEle = Just 9.0, pntTime = Nothing}))
  , (KJCT, ICAOData (Point {pntLat = 30.516666666666666, pntLon = -99.76666666666667, pntEle = Just 533.0, pntTime = Nothing}))
  , (KJFK, ICAOData (Point {pntLat = 40.63333333333333, pntLon = -73.76666666666667, pntEle = Just 4.0, pntTime = Nothing}))
  , (KLAN, ICAOData (Point {pntLat = 42.78333333333333, pntLon = -84.58333333333333, pntEle = Just 262.0, pntTime = Nothing}))
  , (KLAS, ICAOData (Point {pntLat = 36.11666666666667, pntLon = -115.26666666666667, pntEle = Just 663.0, pntTime = Nothing}))
  , (KLAX, ICAOData (Point {pntLat = 33.93333333333333, pntLon = -118.38333333333334, pntEle = Just 38.0, pntTime = Nothing}))
  , (KLBB, ICAOData (Point {pntLat = 33.666666666666664, pntLon = -101.81666666666666, pntEle = Just 1000.0, pntTime = Nothing}))
  , (KLBF, ICAOData (Point {pntLat = 41.11666666666667, pntLon = -100.66666666666667, pntEle = Just 847.0, pntTime = Nothing}))
  , (KLCH, ICAOData (Point {pntLat = 30.2, pntLon = -93.38333333333334, pntEle = Just 5.0, pntTime = Nothing}))
  , (KLEX, ICAOData (Point {pntLat = 38.03333333333333, pntLon = -84.6, pntEle = Just 299.0, pntTime = Nothing}))
  , (KLFI, ICAOData (Point {pntLat = 37.083333333333336, pntLon = -76.35, pntEle = Just 3.0, pntTime = Nothing}))
  , (KLGA, ICAOData (Point {pntLat = 40.78333333333333, pntLon = -73.88333333333334, pntEle = Just 7.0, pntTime = Nothing}))
  , (KLGB, ICAOData (Point {pntLat = 33.81666666666667, pntLon = -118.15, pntEle = Just 17.0, pntTime = Nothing}))
  , (KLHW, ICAOData (Point {pntLat = 31.883333333333333, pntLon = -81.56666666666666, pntEle = Just 14.0, pntTime = Nothing}))
  , (KLIX, ICAOData (Point {pntLat = 30.333333333333332, pntLon = -89.81666666666666, pntEle = Just 8.0, pntTime = Nothing}))
  , (KLND, ICAOData (Point {pntLat = 42.81666666666667, pntLon = -108.73333333333333, pntEle = Just 1703.0, pntTime = Nothing}))
  , (KLNK, ICAOData (Point {pntLat = 40.833333333333336, pntLon = -96.76666666666667, pntEle = Just 372.0, pntTime = Nothing}))
  , (KLRD, ICAOData (Point {pntLat = 27.533333333333335, pntLon = -99.45, pntEle = Just 155.0, pntTime = Nothing}))
  , (KLSE, ICAOData (Point {pntLat = 43.88333333333333, pntLon = -91.25, pntEle = Just 199.0, pntTime = Nothing}))
  , (KLSF, ICAOData (Point {pntLat = 32.333333333333336, pntLon = -84.83333333333333, pntEle = Just 71.0, pntTime = Nothing}))
  , (KLTS, ICAOData (Point {pntLat = 34.65, pntLon = -99.26666666666667, pntEle = Just 420.0, pntTime = Nothing}))
  , (KLWS, ICAOData (Point {pntLat = 46.36666666666667, pntLon = -117.01666666666667, pntEle = Just 438.0, pntTime = Nothing}))
  , (KLYH, ICAOData (Point {pntLat = 37.31666666666667, pntLon = -79.2, pntEle = Just 286.0, pntTime = Nothing}))
  , (KLZK, ICAOData (Point {pntLat = 34.833333333333336, pntLon = -92.25, pntEle = Just 165.0, pntTime = Nothing}))
  , (KMAF, ICAOData (Point {pntLat = 31.933333333333334, pntLon = -102.2, pntEle = Just 875.0, pntTime = Nothing}))
  , (KMCF, ICAOData (Point {pntLat = 27.85, pntLon = -82.5, pntEle = Just 4.0, pntTime = Nothing}))
  , (KMCI, ICAOData (Point {pntLat = 39.3, pntLon = -94.73333333333333, pntEle = Just 313.0, pntTime = Nothing}))
  , (KMCN, ICAOData (Point {pntLat = 32.68333333333333, pntLon = -83.65, pntEle = Just 108.0, pntTime = Nothing}))
  , (KMCO, ICAOData (Point {pntLat = 28.416666666666668, pntLon = -81.33333333333333, pntEle = Just 29.0, pntTime = Nothing}))
  , (KMDW, ICAOData (Point {pntLat = 41.78333333333333, pntLon = -87.75, pntEle = Just 189.0, pntTime = Nothing}))
  , (KMEI, ICAOData (Point {pntLat = 32.333333333333336, pntLon = -88.75, pntEle = Just 91.0, pntTime = Nothing}))
  , (KMEM, ICAOData (Point {pntLat = 35.03333333333333, pntLon = -89.98333333333333, pntEle = Just 102.0, pntTime = Nothing}))
  , (KMER, ICAOData (Point {pntLat = 37.36666666666667, pntLon = -120.56666666666666, pntEle = Just 57.0, pntTime = Nothing}))
  , (KMFR, ICAOData (Point {pntLat = 42.38333333333333, pntLon = -122.88333333333334, pntEle = Just 406.0, pntTime = Nothing}))
  , (KMGE, ICAOData (Point {pntLat = 33.916666666666664, pntLon = -84.51666666666667, pntEle = Just 326.0, pntTime = Nothing}))
  , (KMGM, ICAOData (Point {pntLat = 32.3, pntLon = -86.4, pntEle = Just 67.0, pntTime = Nothing}))
  , (KMHX, ICAOData (Point {pntLat = 34.78333333333333, pntLon = -76.88333333333334, pntEle = Just 11.0, pntTime = Nothing}))
  , (KMIA, ICAOData (Point {pntLat = 25.783333333333335, pntLon = -80.31666666666666, pntEle = Just 4.0, pntTime = Nothing}))
  , (KMKE, ICAOData (Point {pntLat = 42.95, pntLon = -87.9, pntEle = Just 220.0, pntTime = Nothing}))
  , (KMKG, ICAOData (Point {pntLat = 43.166666666666664, pntLon = -86.23333333333333, pntEle = Just 191.0, pntTime = Nothing}))
  , (KMLB, ICAOData (Point {pntLat = 28.1, pntLon = -80.65, pntEle = Just 10.0, pntTime = Nothing}))
  , (KMLI, ICAOData (Point {pntLat = 41.45, pntLon = -90.51666666666667, pntEle = Just 180.0, pntTime = Nothing}))
  , (KMLS, ICAOData (Point {pntLat = 46.43333333333333, pntLon = -105.88333333333334, pntEle = Just 801.0, pntTime = Nothing}))
  , (KMMO, ICAOData (Point {pntLat = 41.36666666666667, pntLon = -88.68333333333334, pntEle = Just 223.0, pntTime = Nothing}))
  , (KMOB, ICAOData (Point {pntLat = 30.683333333333334, pntLon = -88.23333333333333, pntEle = Just 66.0, pntTime = Nothing}))
  , (KMQT, ICAOData (Point {pntLat = 46.53333333333333, pntLon = -87.55, pntEle = Just 434.0, pntTime = Nothing}))
  , (KMRF, ICAOData (Point {pntLat = 30.366666666666667, pntLon = -104.01666666666667, pntEle = Just 1478.0, pntTime = Nothing}))
  , (KMSN, ICAOData (Point {pntLat = 43.13333333333333, pntLon = -89.35, pntEle = Just 263.0, pntTime = Nothing}))
  , (KMSO, ICAOData (Point {pntLat = 46.916666666666664, pntLon = -114.1, pntEle = Just 976.0, pntTime = Nothing}))
  , (KMSP, ICAOData (Point {pntLat = 44.86666666666667, pntLon = -93.21666666666667, pntEle = Just 256.0, pntTime = Nothing}))
  , (KMSY, ICAOData (Point {pntLat = 30.0, pntLon = -90.25, pntEle = Just 2.0, pntTime = Nothing}))
  , (KMWN, ICAOData (Point {pntLat = 44.266666666666666, pntLon = -71.3, pntEle = Just 1909.0, pntTime = Nothing}))
  , (KMWS, ICAOData (Point {pntLat = 34.233333333333334, pntLon = -118.06666666666666, pntEle = Just 1739.0, pntTime = Nothing}))
  , (KMYR, ICAOData (Point {pntLat = 33.68333333333333, pntLon = -78.93333333333334, pntEle = Just 8.0, pntTime = Nothing}))
  , (KN78, ICAOData (Point {pntLat = 39.766666666666666, pntLon = -74.1, pntEle = Just 6.0, pntTime = Nothing}))
  , (KNEL, ICAOData (Point {pntLat = 40.03333333333333, pntLon = -74.31666666666666, pntEle = Just 31.0, pntTime = Nothing}))
  , (KNGZ, ICAOData (Point {pntLat = 37.78333333333333, pntLon = -122.31666666666666, pntEle = Just 4.0, pntTime = Nothing}))
  , (KNHK, ICAOData (Point {pntLat = 38.3, pntLon = -76.4, pntEle = Just 12.0, pntTime = Nothing}))
  , (KNHZ, ICAOData (Point {pntLat = 43.9, pntLon = -69.93333333333334, pntEle = Just 23.0, pntTime = Nothing}))
  , (KNID, ICAOData (Point {pntLat = 35.68333333333333, pntLon = -117.7, pntEle = Just 696.0, pntTime = Nothing}))
  , (KNJK, ICAOData (Point {pntLat = 32.81666666666667, pntLon = -115.65, pntEle = Just (-13.0), pntTime = Nothing}))
  , (KNKT, ICAOData (Point {pntLat = 34.88333333333333, pntLon = -76.86666666666666, pntEle = Just 8.0, pntTime = Nothing}))
  , (KNKX, ICAOData (Point {pntLat = 32.86666666666667, pntLon = -117.15, pntEle = Just 146.0, pntTime = Nothing}))
  , (KNLC, ICAOData (Point {pntLat = 36.333333333333336, pntLon = -119.95, pntEle = Just 71.0, pntTime = Nothing}))
  , (KNOW, ICAOData (Point {pntLat = 48.13333333333333, pntLon = -123.4, pntEle = Just 5.0, pntTime = Nothing}))
  , (KNSI, ICAOData (Point {pntLat = 33.233333333333334, pntLon = -119.46666666666667, pntEle = Just 154.0, pntTime = Nothing}))
  , (KNTD, ICAOData (Point {pntLat = 34.11666666666667, pntLon = -119.11666666666666, pntEle = Just 4.0, pntTime = Nothing}))
  , (KNUQ, ICAOData (Point {pntLat = 37.43333333333333, pntLon = -122.05, pntEle = Just 10.0, pntTime = Nothing}))
  , (KO64, ICAOData (Point {pntLat = 39.4, pntLon = -123.81666666666666, pntEle = Just 19.0, pntTime = Nothing}))
  , (KOAK, ICAOData (Point {pntLat = 37.71666666666667, pntLon = -122.23333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (KOAX, ICAOData (Point {pntLat = 41.31666666666667, pntLon = -96.36666666666666, pntEle = Just 350.0, pntTime = Nothing}))
  , (KOFF, ICAOData (Point {pntLat = 41.11666666666667, pntLon = -95.9, pntEle = Just 319.0, pntTime = Nothing}))
  , (KOFK, ICAOData (Point {pntLat = 41.983333333333334, pntLon = -97.43333333333334, pntEle = Just 479.0, pntTime = Nothing}))
  , (KOGD, ICAOData (Point {pntLat = 41.2, pntLon = -112.01666666666667, pntEle = Just 1362.0, pntTime = Nothing}))
  , (KOKC, ICAOData (Point {pntLat = 35.38333333333333, pntLon = -97.6, pntEle = Just 395.0, pntTime = Nothing}))
  , (KOLM, ICAOData (Point {pntLat = 46.96666666666667, pntLon = -122.9, pntEle = Just 63.0, pntTime = Nothing}))
  , (KOMA, ICAOData (Point {pntLat = 41.3, pntLon = -95.9, pntEle = Just 300.0, pntTime = Nothing}))
  , (KONM, ICAOData (Point {pntLat = 34.016666666666666, pntLon = -106.9, pntEle = Just 1478.0, pntTime = Nothing}))
  , (KORD, ICAOData (Point {pntLat = 41.983333333333334, pntLon = -87.91666666666667, pntEle = Just 204.0, pntTime = Nothing}))
  , (KORF, ICAOData (Point {pntLat = 36.9, pntLon = -76.2, pntEle = Just 8.0, pntTime = Nothing}))
  , (KOUN, ICAOData (Point {pntLat = 35.21666666666667, pntLon = -97.45, pntEle = Just 357.0, pntTime = Nothing}))
  , (KP07, ICAOData (Point {pntLat = 30.166666666666668, pntLon = -102.41666666666667, pntEle = Just 865.0, pntTime = Nothing}))
  , (KP11, ICAOData (Point {pntLat = 48.1, pntLon = -98.86666666666666, pntEle = Just 439.0, pntTime = Nothing}))
  , (KP24, ICAOData (Point {pntLat = 47.75, pntLon = -101.83333333333333, pntEle = Just 624.0, pntTime = Nothing}))
  , (KP28, ICAOData (Point {pntLat = 37.28333333333333, pntLon = -98.55, pntEle = Just 469.0, pntTime = Nothing}))
  , (KP35, ICAOData (Point {pntLat = 40.25, pntLon = -93.71666666666667, pntEle = Just 271.0, pntTime = Nothing}))
  , (KP38, ICAOData (Point {pntLat = 37.6, pntLon = -114.51666666666667, pntEle = Just 1333.0, pntTime = Nothing}))
  , (KP39, ICAOData (Point {pntLat = 46.6, pntLon = -94.31666666666666, pntEle = Just 390.0, pntTime = Nothing}))
  , (KPAH, ICAOData (Point {pntLat = 37.05, pntLon = -88.76666666666667, pntEle = Just 125.0, pntTime = Nothing}))
  , (KPAM, ICAOData (Point {pntLat = 30.066666666666666, pntLon = -85.58333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (KPBI, ICAOData (Point {pntLat = 26.683333333333334, pntLon = -80.1, pntEle = Just 6.0, pntTime = Nothing}))
  , (KPDT, ICAOData (Point {pntLat = 46.166666666666664, pntLon = -119.38333333333334, pntEle = Just 455.0, pntTime = Nothing}))
  , (KPDX, ICAOData (Point {pntLat = 45.583333333333336, pntLon = -122.6, pntEle = Just 8.0, pntTime = Nothing}))
  , (KPGA, ICAOData (Point {pntLat = 36.93333333333333, pntLon = -111.45, pntEle = Just 1314.0, pntTime = Nothing}))
  , (KPHL, ICAOData (Point {pntLat = 39.86666666666667, pntLon = -75.23333333333333, pntEle = Just 6.0, pntTime = Nothing}))
  , (KPHX, ICAOData (Point {pntLat = 33.43333333333333, pntLon = -112.05, pntEle = Just 345.0, pntTime = Nothing}))
  , (KPIA, ICAOData (Point {pntLat = 40.666666666666664, pntLon = -89.68333333333334, pntEle = Just 201.0, pntTime = Nothing}))
  , (KPIH, ICAOData (Point {pntLat = 42.916666666666664, pntLon = -112.6, pntEle = Just 1356.0, pntTime = Nothing}))
  , (KPIT, ICAOData (Point {pntLat = 40.483333333333334, pntLon = -80.23333333333333, pntEle = Just 367.0, pntTime = Nothing}))
  , (KPKF, ICAOData (Point {pntLat = 45.93333333333333, pntLon = -90.45, pntEle = Just 462.0, pntTime = Nothing}))
  , (KPMD, ICAOData (Point {pntLat = 34.63333333333333, pntLon = -118.08333333333333, pntEle = Just 775.0, pntTime = Nothing}))
  , (KPOB, ICAOData (Point {pntLat = 35.166666666666664, pntLon = -79.03333333333333, pntEle = Just 61.0, pntTime = Nothing}))
  , (KPOE, ICAOData (Point {pntLat = 31.05, pntLon = -93.18333333333334, pntEle = Just 101.0, pntTime = Nothing}))
  , (KPQI, ICAOData (Point {pntLat = 46.68333333333333, pntLon = -68.05, pntEle = Just 163.0, pntTime = Nothing}))
  , (KPTT, ICAOData (Point {pntLat = 37.7, pntLon = -98.75, pntEle = Just 595.0, pntTime = Nothing}))
  , (KPUB, ICAOData (Point {pntLat = 38.28333333333333, pntLon = -104.5, pntEle = Just 1440.0, pntTime = Nothing}))
  , (KPUC, ICAOData (Point {pntLat = 39.61666666666667, pntLon = -110.75, pntEle = Just 1814.0, pntTime = Nothing}))
  , (KPVD, ICAOData (Point {pntLat = 41.733333333333334, pntLon = -71.43333333333334, pntEle = Just 17.0, pntTime = Nothing}))
  , (KPWM, ICAOData (Point {pntLat = 43.65, pntLon = -70.3, pntEle = Just 23.0, pntTime = Nothing}))
  , (KQHA, ICAOData (Point {pntLat = 19.666666666666668, pntLon = -73.03333333333333, pntEle = Just 17.0, pntTime = Nothing}))
  , (KRAP, ICAOData (Point {pntLat = 44.05, pntLon = -103.05, pntEle = Just 976.0, pntTime = Nothing}))
  , (KRBL, ICAOData (Point {pntLat = 40.15, pntLon = -122.25, pntEle = Just 106.0, pntTime = Nothing}))
  , (KRDD, ICAOData (Point {pntLat = 40.5, pntLon = -122.28333333333333, pntEle = Just 153.0, pntTime = Nothing}))
  , (KRDU, ICAOData (Point {pntLat = 35.9, pntLon = -78.76666666666667, pntEle = Just 133.0, pntTime = Nothing}))
  , (KRFD, ICAOData (Point {pntLat = 42.2, pntLon = -89.1, pntEle = Just 224.0, pntTime = Nothing}))
  , (KRIC, ICAOData (Point {pntLat = 37.516666666666666, pntLon = -77.31666666666666, pntEle = Just 51.0, pntTime = Nothing}))
  , (KRIV, ICAOData (Point {pntLat = 33.9, pntLon = -117.25, pntEle = Just 469.0, pntTime = Nothing}))
  , (KRIW, ICAOData (Point {pntLat = 43.06666666666667, pntLon = -108.45, pntEle = Just 1684.0, pntTime = Nothing}))
  , (KRKS, ICAOData (Point {pntLat = 41.6, pntLon = -109.05, pntEle = Just 2060.0, pntTime = Nothing}))
  , (KRMG, ICAOData (Point {pntLat = 34.35, pntLon = -85.16666666666667, pntEle = Just 195.0, pntTime = Nothing}))
  , (KRNO, ICAOData (Point {pntLat = 39.483333333333334, pntLon = -119.76666666666667, pntEle = Just 1345.0, pntTime = Nothing}))
  , (KROA, ICAOData (Point {pntLat = 37.31666666666667, pntLon = -79.96666666666667, pntEle = Just 358.0, pntTime = Nothing}))
  , (KROC, ICAOData (Point {pntLat = 43.11666666666667, pntLon = -77.68333333333334, pntEle = Just 170.0, pntTime = Nothing}))
  , (KROW, ICAOData (Point {pntLat = 33.3, pntLon = -104.53333333333333, pntEle = Just 1118.0, pntTime = Nothing}))
  , (KRQE, ICAOData (Point {pntLat = 35.65, pntLon = -109.06666666666666, pntEle = Just 2054.0, pntTime = Nothing}))
  , (KRST, ICAOData (Point {pntLat = 43.9, pntLon = -92.5, pntEle = Just 401.0, pntTime = Nothing}))
  , (KRUE, ICAOData (Point {pntLat = 35.25, pntLon = -93.1, pntEle = Just 123.0, pntTime = Nothing}))
  , (KRUM, ICAOData (Point {pntLat = 44.53333333333333, pntLon = -70.53333333333333, pntEle = Just 192.0, pntTime = Nothing}))
  , (KSAC, ICAOData (Point {pntLat = 38.5, pntLon = -121.5, pntEle = Just 6.0, pntTime = Nothing}))
  , (KSAN, ICAOData (Point {pntLat = 32.733333333333334, pntLon = -117.18333333333334, pntEle = Just 5.0, pntTime = Nothing}))
  , (KSAT, ICAOData (Point {pntLat = 29.533333333333335, pntLon = -98.46666666666667, pntEle = Just 247.0, pntTime = Nothing}))
  , (KSAV, ICAOData (Point {pntLat = 32.11666666666667, pntLon = -81.2, pntEle = Just 16.0, pntTime = Nothing}))
  , (KSBN, ICAOData (Point {pntLat = 41.7, pntLon = -86.31666666666666, pntEle = Just 244.0, pntTime = Nothing}))
  , (KSCK, ICAOData (Point {pntLat = 37.9, pntLon = -121.23333333333333, pntEle = Just 9.0, pntTime = Nothing}))
  , (KSDB, ICAOData (Point {pntLat = 34.75, pntLon = -118.71666666666667, pntEle = Just 1379.0, pntTime = Nothing}))
  , (KSDF, ICAOData (Point {pntLat = 38.18333333333333, pntLon = -85.73333333333333, pntEle = Just 151.0, pntTime = Nothing}))
  , (KSEA, ICAOData (Point {pntLat = 47.45, pntLon = -122.31666666666666, pntEle = Just 131.0, pntTime = Nothing}))
  , (KSEP, ICAOData (Point {pntLat = 32.21666666666667, pntLon = -98.18333333333334, pntEle = Just 403.0, pntTime = Nothing}))
  , (KSFD, ICAOData (Point {pntLat = 43.38333333333333, pntLon = -99.83333333333333, pntEle = Just 619.0, pntTime = Nothing}))
  , (KSFO, ICAOData (Point {pntLat = 37.61666666666667, pntLon = -122.36666666666666, pntEle = Just 3.0, pntTime = Nothing}))
  , (KSGF, ICAOData (Point {pntLat = 37.233333333333334, pntLon = -93.38333333333334, pntEle = Just 386.0, pntTime = Nothing}))
  , (KSHR, ICAOData (Point {pntLat = 44.766666666666666, pntLon = -106.96666666666667, pntEle = Just 1226.0, pntTime = Nothing}))
  , (KSHV, ICAOData (Point {pntLat = 32.45, pntLon = -93.83333333333333, pntEle = Just 79.0, pntTime = Nothing}))
  , (KSJT, ICAOData (Point {pntLat = 31.35, pntLon = -100.5, pntEle = Just 584.0, pntTime = Nothing}))
  , (KSLC, ICAOData (Point {pntLat = 40.78333333333333, pntLon = -111.96666666666667, pntEle = Just 1288.0, pntTime = Nothing}))
  , (KSLE, ICAOData (Point {pntLat = 44.9, pntLon = -123.0, pntEle = Just 64.0, pntTime = Nothing}))
  , (KSLO, ICAOData (Point {pntLat = 38.63333333333333, pntLon = -88.95, pntEle = Just 175.0, pntTime = Nothing}))
  , (KSMX, ICAOData (Point {pntLat = 34.9, pntLon = -120.46666666666667, pntEle = Just 79.0, pntTime = Nothing}))
  , (KSNY, ICAOData (Point {pntLat = 41.1, pntLon = -102.98333333333333, pntEle = Just 1315.0, pntTime = Nothing}))
  , (KSPS, ICAOData (Point {pntLat = 33.983333333333334, pntLon = -98.5, pntEle = Just 309.0, pntTime = Nothing}))
  , (KSSC, ICAOData (Point {pntLat = 33.96666666666667, pntLon = -80.48333333333333, pntEle = Just 74.0, pntTime = Nothing}))
  , (KSTJ, ICAOData (Point {pntLat = 39.766666666666666, pntLon = -94.91666666666667, pntEle = Just 252.0, pntTime = Nothing}))
  , (KSTL, ICAOData (Point {pntLat = 38.75, pntLon = -90.36666666666666, pntEle = Just 184.0, pntTime = Nothing}))
  , (KSUU, ICAOData (Point {pntLat = 38.266666666666666, pntLon = -121.95, pntEle = Just 19.0, pntTime = Nothing}))
  , (KSUX, ICAOData (Point {pntLat = 42.38333333333333, pntLon = -96.38333333333334, pntEle = Just 335.0, pntTime = Nothing}))
  , (KSVC, ICAOData (Point {pntLat = 32.63333333333333, pntLon = -108.15, pntEle = Just 1659.0, pntTime = Nothing}))
  , (KSYR, ICAOData (Point {pntLat = 43.11666666666667, pntLon = -76.1, pntEle = Just 125.0, pntTime = Nothing}))
  , (KTBW, ICAOData (Point {pntLat = 27.7, pntLon = -82.4, pntEle = Just 12.0, pntTime = Nothing}))
  , (KTCM, ICAOData (Point {pntLat = 47.15, pntLon = -122.48333333333333, pntEle = Just 98.0, pntTime = Nothing}))
  , (KTCS, ICAOData (Point {pntLat = 33.233333333333334, pntLon = -107.26666666666667, pntEle = Just 1478.0, pntTime = Nothing}))
  , (KTIK, ICAOData (Point {pntLat = 35.416666666666664, pntLon = -97.38333333333334, pntEle = Just 394.0, pntTime = Nothing}))
  , (KTLH, ICAOData (Point {pntLat = 30.4, pntLon = -84.35, pntEle = Just 25.0, pntTime = Nothing}))
  , (KTOL, ICAOData (Point {pntLat = 41.583333333333336, pntLon = -83.8, pntEle = Just 208.0, pntTime = Nothing}))
  , (KTOP, ICAOData (Point {pntLat = 39.06666666666667, pntLon = -95.63333333333334, pntEle = Just 269.0, pntTime = Nothing}))
  , (KTPA, ICAOData (Point {pntLat = 27.966666666666665, pntLon = -82.53333333333333, pntEle = Just 8.0, pntTime = Nothing}))
  , (KTUL, ICAOData (Point {pntLat = 36.2, pntLon = -95.88333333333334, pntEle = Just 206.0, pntTime = Nothing}))
  , (KTUP, ICAOData (Point {pntLat = 34.266666666666666, pntLon = -88.76666666666667, pntEle = Just 105.0, pntTime = Nothing}))
  , (KTUS, ICAOData (Point {pntLat = 32.13333333333333, pntLon = -110.95, pntEle = Just 805.0, pntTime = Nothing}))
  , (KTYS, ICAOData (Point {pntLat = 35.81666666666667, pntLon = -83.98333333333333, pntEle = Just 299.0, pntTime = Nothing}))
  , (KU67, ICAOData (Point {pntLat = 40.3, pntLon = -109.98333333333333, pntEle = Just 1553.0, pntTime = Nothing}))
  , (KUIL, ICAOData (Point {pntLat = 47.93333333333333, pntLon = -124.56666666666666, pntEle = Just 59.0, pntTime = Nothing}))
  , (KVAD, ICAOData (Point {pntLat = 30.966666666666665, pntLon = -83.2, pntEle = Just 71.0, pntTime = Nothing}))
  , (KVBG, ICAOData (Point {pntLat = 34.71666666666667, pntLon = -120.56666666666666, pntEle = Just 112.0, pntTime = Nothing}))
  , (KVCT, ICAOData (Point {pntLat = 28.866666666666667, pntLon = -96.93333333333334, pntEle = Just 35.0, pntTime = Nothing}))
  , (KVPS, ICAOData (Point {pntLat = 30.483333333333334, pntLon = -86.51666666666667, pntEle = Just 26.0, pntTime = Nothing}))
  , (KVTN, ICAOData (Point {pntLat = 42.86666666666667, pntLon = -100.55, pntEle = Just 789.0, pntTime = Nothing}))
  , (KW30, ICAOData (Point {pntLat = 38.333333333333336, pntLon = -75.08333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (KWAL, ICAOData (Point {pntLat = 37.93333333333333, pntLon = -75.46666666666667, pntEle = Just 12.0, pntTime = Nothing}))
  , (KWDD, ICAOData (Point {pntLat = 42.3, pntLon = -82.7, pntEle = Just 184.0, pntTime = Nothing}))
  , (KWMC, ICAOData (Point {pntLat = 40.9, pntLon = -117.8, pntEle = Just 1312.0, pntTime = Nothing}))
  , (KWWR, ICAOData (Point {pntLat = 36.43333333333333, pntLon = -99.51666666666667, pntEle = Just 667.0, pntTime = Nothing}))
  , (KXMR, ICAOData (Point {pntLat = 28.466666666666665, pntLon = -80.56666666666666, pntEle = Just 3.0, pntTime = Nothing}))
  , (KYKM, ICAOData (Point {pntLat = 46.56666666666667, pntLon = -120.53333333333333, pntEle = Just 334.0, pntTime = Nothing}))
  , (KYNG, ICAOData (Point {pntLat = 41.25, pntLon = -80.66666666666667, pntEle = Just 365.0, pntTime = Nothing}))
  , (KYUM, ICAOData (Point {pntLat = 32.65, pntLon = -114.6, pntEle = Just 65.0, pntTime = Nothing}))
  , (LATI, ICAOData (Point {pntLat = 41.333333333333336, pntLon = 19.783333333333335, pntEle = Just 89.0, pntTime = Nothing}))
  , (LBBG, ICAOData (Point {pntLat = 42.483333333333334, pntLon = 27.483333333333334, pntEle = Just 16.0, pntTime = Nothing}))
  , (LBPD, ICAOData (Point {pntLat = 42.13333333333333, pntLon = 24.75, pntEle = Just 179.0, pntTime = Nothing}))
  , (LBRS, ICAOData (Point {pntLat = 43.85, pntLon = 25.95, pntEle = Just 37.0, pntTime = Nothing}))
  , (LBSF, ICAOData (Point {pntLat = 42.65, pntLon = 23.383333333333333, pntEle = Just 586.0, pntTime = Nothing}))
  , (LBWN, ICAOData (Point {pntLat = 43.2, pntLon = 27.916666666666668, pntEle = Just 41.0, pntTime = Nothing}))
  , (LCLK, ICAOData (Point {pntLat = 34.88333333333333, pntLon = 33.63333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (LCNC, ICAOData (Point {pntLat = 35.15, pntLon = 33.4, pntEle = Just 162.0, pntTime = Nothing}))
  , (LCPH, ICAOData (Point {pntLat = 34.71666666666667, pntLon = 32.483333333333334, pntEle = Just 11.0, pntTime = Nothing}))
  , (LCRA, ICAOData (Point {pntLat = 34.583333333333336, pntLon = 32.983333333333334, pntEle = Just 23.0, pntTime = Nothing}))
  , (LDDD, ICAOData (Point {pntLat = 45.81666666666667, pntLon = 16.033333333333335, pntEle = Just 123.0, pntTime = Nothing}))
  , (LDDU, ICAOData (Point {pntLat = 42.56666666666667, pntLon = 18.266666666666666, pntEle = Just 157.0, pntTime = Nothing}))
  , (LDOR, ICAOData (Point {pntLat = 45.166666666666664, pntLon = 18.0, pntEle = Just 88.0, pntTime = Nothing}))
  , (LDOS, ICAOData (Point {pntLat = 45.45, pntLon = 18.8, pntEle = Just 88.0, pntTime = Nothing}))
  , (LDPL, ICAOData (Point {pntLat = 45.9, pntLon = 13.916666666666666, pntEle = Just 63.0, pntTime = Nothing}))
  , (LDRI, ICAOData (Point {pntLat = 45.21666666666667, pntLon = 14.583333333333334, pntEle = Just 85.0, pntTime = Nothing}))
  , (LDSH, ICAOData (Point {pntLat = 43.166666666666664, pntLon = 16.45, pntEle = Just 20.0, pntTime = Nothing}))
  , (LDSP, ICAOData (Point {pntLat = 43.53333333333333, pntLon = 16.3, pntEle = Just 19.0, pntTime = Nothing}))
  , (LDVA, ICAOData (Point {pntLat = 46.3, pntLon = 16.383333333333333, pntEle = Just 167.0, pntTime = Nothing}))
  , (LDZA, ICAOData (Point {pntLat = 45.733333333333334, pntLon = 16.066666666666666, pntEle = Just 106.0, pntTime = Nothing}))
  , (LDZD, ICAOData (Point {pntLat = 44.1, pntLon = 15.35, pntEle = Just 82.0, pntTime = Nothing}))
  , (LEAB, ICAOData (Point {pntLat = 38.95, pntLon = -1.85, pntEle = Just 702.0, pntTime = Nothing}))
  , (LEAL, ICAOData (Point {pntLat = 38.28333333333333, pntLon = -0.55, pntEle = Just 43.0, pntTime = Nothing}))
  , (LEAM, ICAOData (Point {pntLat = 36.85, pntLon = -2.3833333333333333, pntEle = Just 15.0, pntTime = Nothing}))
  , (LEAS, ICAOData (Point {pntLat = 43.55, pntLon = -6.033333333333333, pntEle = Just 127.0, pntTime = Nothing}))
  , (LEBA, ICAOData (Point {pntLat = 37.85, pntLon = -4.85, pntEle = Just 90.0, pntTime = Nothing}))
  , (LEBB, ICAOData (Point {pntLat = 43.3, pntLon = -2.9333333333333336, pntEle = Just 42.0, pntTime = Nothing}))
  , (LEBG, ICAOData (Point {pntLat = 42.36666666666667, pntLon = -3.6333333333333333, pntEle = Just 894.0, pntTime = Nothing}))
  , (LEBL, ICAOData (Point {pntLat = 41.28333333333333, pntLon = 2.066666666666667, pntEle = Just 4.0, pntTime = Nothing}))
  , (LEBZ, ICAOData (Point {pntLat = 38.88333333333333, pntLon = -6.816666666666666, pntEle = Just 185.0, pntTime = Nothing}))
  , (LECH, ICAOData (Point {pntLat = 40.93333333333333, pntLon = -1.3, pntEle = Just 890.0, pntTime = Nothing}))
  , (LECO, ICAOData (Point {pntLat = 43.3, pntLon = -8.383333333333333, pntEle = Just 97.0, pntTime = Nothing}))
  , (LECV, ICAOData (Point {pntLat = 40.65, pntLon = -3.7333333333333334, pntEle = Nothing, pntTime = Nothing}))
  , (LEGE, ICAOData (Point {pntLat = 41.9, pntLon = 2.7666666666666666, pntEle = Just 143.0, pntTime = Nothing}))
  , (LEGR, ICAOData (Point {pntLat = 37.18333333333333, pntLon = -3.783333333333333, pntEle = Just 567.0, pntTime = Nothing}))
  , (LEGT, ICAOData (Point {pntLat = 40.3, pntLon = -3.716666666666667, pntEle = Just 620.0, pntTime = Nothing}))
  , (LEHI, ICAOData (Point {pntLat = 38.516666666666666, pntLon = -5.116666666666666, pntEle = Just 540.0, pntTime = Nothing}))
  , (LEIB, ICAOData (Point {pntLat = 38.86666666666667, pntLon = 1.3833333333333333, pntEle = Just 7.0, pntTime = Nothing}))
  , (LEJR, ICAOData (Point {pntLat = 36.75, pntLon = -6.066666666666666, pntEle = Just 27.0, pntTime = Nothing}))
  , (LELC, ICAOData (Point {pntLat = 37.78333333333333, pntLon = -0.8, pntEle = Just 5.0, pntTime = Nothing}))
  , (LELN, ICAOData (Point {pntLat = 42.583333333333336, pntLon = -5.65, pntEle = Just 926.0, pntTime = Nothing}))
  , (LELO, ICAOData (Point {pntLat = 42.45, pntLon = -2.3333333333333335, pntEle = Just 353.0, pntTime = Nothing}))
  , (LEMD, ICAOData (Point {pntLat = 40.45, pntLon = -3.55, pntEle = Just 609.0, pntTime = Nothing}))
  , (LEMG, ICAOData (Point {pntLat = 36.666666666666664, pntLon = -4.483333333333333, pntEle = Just 16.0, pntTime = Nothing}))
  , (LEMH, ICAOData (Point {pntLat = 39.86666666666667, pntLon = 4.233333333333333, pntEle = Just 87.0, pntTime = Nothing}))
  , (LEMO, ICAOData (Point {pntLat = 37.15, pntLon = -5.616666666666667, pntEle = Just 87.0, pntTime = Nothing}))
  , (LEPA, ICAOData (Point {pntLat = 39.55, pntLon = 2.7333333333333334, pntEle = Just 4.0, pntTime = Nothing}))
  , (LEPP, ICAOData (Point {pntLat = 42.766666666666666, pntLon = -1.6333333333333333, pntEle = Just 459.0, pntTime = Nothing}))
  , (LERI, ICAOData (Point {pntLat = 37.95, pntLon = -1.2333333333333334, pntEle = Just 75.0, pntTime = Nothing}))
  , (LERS, ICAOData (Point {pntLat = 41.15, pntLon = 1.1666666666666667, pntEle = Just 71.0, pntTime = Nothing}))
  , (LERT, ICAOData (Point {pntLat = 36.65, pntLon = -6.35, pntEle = Just 26.0, pntTime = Nothing}))
  , (LESA, ICAOData (Point {pntLat = 40.95, pntLon = -5.5, pntEle = Just 793.0, pntTime = Nothing}))
  , (LESO, ICAOData (Point {pntLat = 43.35, pntLon = -1.8, pntEle = Just 5.0, pntTime = Nothing}))
  , (LEST, ICAOData (Point {pntLat = 42.9, pntLon = -8.433333333333334, pntEle = Just 370.0, pntTime = Nothing}))
  , (LETO, ICAOData (Point {pntLat = 40.483333333333334, pntLon = -3.45, pntEle = Just 607.0, pntTime = Nothing}))
  , (LEVC, ICAOData (Point {pntLat = 39.5, pntLon = -0.4666666666666667, pntEle = Just 69.0, pntTime = Nothing}))
  , (LEVD, ICAOData (Point {pntLat = 41.71666666666667, pntLon = -4.85, pntEle = Just 849.0, pntTime = Nothing}))
  , (LEVS, ICAOData (Point {pntLat = 40.38333333333333, pntLon = -3.783333333333333, pntEle = Just 690.0, pntTime = Nothing}))
  , (LEVT, ICAOData (Point {pntLat = 42.88333333333333, pntLon = -2.716666666666667, pntEle = Just 513.0, pntTime = Nothing}))
  , (LEVX, ICAOData (Point {pntLat = 42.21666666666667, pntLon = -8.633333333333333, pntEle = Just 264.0, pntTime = Nothing}))
  , (LEXJ, ICAOData (Point {pntLat = 43.43333333333333, pntLon = -3.8166666666666664, pntEle = Just 6.0, pntTime = Nothing}))
  , (LEZG, ICAOData (Point {pntLat = 41.666666666666664, pntLon = -1.0166666666666666, pntEle = Just 263.0, pntTime = Nothing}))
  , (LEZL, ICAOData (Point {pntLat = 37.416666666666664, pntLon = -5.9, pntEle = Just 34.0, pntTime = Nothing}))
  , (LFAT, ICAOData (Point {pntLat = 50.516666666666666, pntLon = 1.6166666666666667, pntEle = Just 10.0, pntTime = Nothing}))
  , (LFBA, ICAOData (Point {pntLat = 44.18333333333333, pntLon = 0.6, pntEle = Just 61.0, pntTime = Nothing}))
  , (LFBC, ICAOData (Point {pntLat = 44.53333333333333, pntLon = -1.1333333333333333, pntEle = Just 25.0, pntTime = Nothing}))
  , (LFBD, ICAOData (Point {pntLat = 44.833333333333336, pntLon = -0.7, pntEle = Just 49.0, pntTime = Nothing}))
  , (LFBE, ICAOData (Point {pntLat = 44.81666666666667, pntLon = 0.5166666666666667, pntEle = Just 51.0, pntTime = Nothing}))
  , (LFBF, ICAOData (Point {pntLat = 43.53333333333333, pntLon = 1.3666666666666667, pntEle = Just 164.0, pntTime = Nothing}))
  , (LFBG, ICAOData (Point {pntLat = 45.666666666666664, pntLon = -0.31666666666666665, pntEle = Just 30.0, pntTime = Nothing}))
  , (LFBH, ICAOData (Point {pntLat = 46.15, pntLon = -1.15, pntEle = Just 4.0, pntTime = Nothing}))
  , (LFBI, ICAOData (Point {pntLat = 46.583333333333336, pntLon = 0.31666666666666665, pntEle = Just 129.0, pntTime = Nothing}))
  , (LFBL, ICAOData (Point {pntLat = 45.86666666666667, pntLon = 1.1833333333333333, pntEle = Just 396.0, pntTime = Nothing}))
  , (LFBM, ICAOData (Point {pntLat = 43.916666666666664, pntLon = -0.5, pntEle = Just 62.0, pntTime = Nothing}))
  , (LFBN, ICAOData (Point {pntLat = 46.31666666666667, pntLon = -0.4, pntEle = Just 61.0, pntTime = Nothing}))
  , (LFBO, ICAOData (Point {pntLat = 43.63333333333333, pntLon = 1.3666666666666667, pntEle = Just 152.0, pntTime = Nothing}))
  , (LFBP, ICAOData (Point {pntLat = 43.38333333333333, pntLon = -0.4166666666666667, pntEle = Just 188.0, pntTime = Nothing}))
  , (LFBS, ICAOData (Point {pntLat = 44.43333333333333, pntLon = -1.25, pntEle = Just 33.0, pntTime = Nothing}))
  , (LFBT, ICAOData (Point {pntLat = 43.18333333333333, pntLon = -0.0, pntEle = Just 379.0, pntTime = Nothing}))
  , (LFBV, ICAOData (Point {pntLat = 45.15, pntLon = 1.4666666666666668, pntEle = Just 111.0, pntTime = Nothing}))
  , (LFBX, ICAOData (Point {pntLat = 45.2, pntLon = 0.8166666666666667, pntEle = Just 100.0, pntTime = Nothing}))
  , (LFBY, ICAOData (Point {pntLat = 43.68333333333333, pntLon = -1.0666666666666667, pntEle = Just 32.0, pntTime = Nothing}))
  , (LFBZ, ICAOData (Point {pntLat = 43.46666666666667, pntLon = -1.5333333333333332, pntEle = Just 75.0, pntTime = Nothing}))
  , (LFCG, ICAOData (Point {pntLat = 43.0, pntLon = 1.1, pntEle = Just 414.0, pntTime = Nothing}))
  , (LFCI, ICAOData (Point {pntLat = 43.916666666666664, pntLon = 2.1166666666666667, pntEle = Just 172.0, pntTime = Nothing}))
  , (LFCR, ICAOData (Point {pntLat = 44.4, pntLon = 2.4833333333333334, pntEle = Just 581.0, pntTime = Nothing}))
  , (LFDH, ICAOData (Point {pntLat = 43.68333333333333, pntLon = 0.6, pntEle = Just 125.0, pntTime = Nothing}))
  , (LFHP, ICAOData (Point {pntLat = 45.083333333333336, pntLon = 3.7666666666666666, pntEle = Just 832.0, pntTime = Nothing}))
  , (LFIG, ICAOData (Point {pntLat = 44.18333333333333, pntLon = 2.5166666666666666, pntEle = Just 1020.0, pntTime = Nothing}))
  , (LFJL, ICAOData (Point {pntLat = 48.983333333333334, pntLon = 6.25, pntEle = Just 264.0, pntTime = Nothing}))
  , (LFKB, ICAOData (Point {pntLat = 42.55, pntLon = 9.483333333333333, pntEle = Just 8.0, pntTime = Nothing}))
  , (LFKC, ICAOData (Point {pntLat = 42.53333333333333, pntLon = 8.8, pntEle = Just 64.0, pntTime = Nothing}))
  , (LFKF, ICAOData (Point {pntLat = 41.5, pntLon = 9.1, pntEle = Just 26.0, pntTime = Nothing}))
  , (LFKJ, ICAOData (Point {pntLat = 41.916666666666664, pntLon = 8.8, pntEle = Just 6.0, pntTime = Nothing}))
  , (LFKS, ICAOData (Point {pntLat = 41.916666666666664, pntLon = 9.4, pntEle = Just 8.0, pntTime = Nothing}))
  , (LFLA, ICAOData (Point {pntLat = 47.8, pntLon = 3.55, pntEle = Just 207.0, pntTime = Nothing}))
  , (LFLB, ICAOData (Point {pntLat = 45.65, pntLon = 5.883333333333333, pntEle = Just 235.0, pntTime = Nothing}))
  , (LFLC, ICAOData (Point {pntLat = 45.78333333333333, pntLon = 3.1666666666666665, pntEle = Just 332.0, pntTime = Nothing}))
  , (LFLD, ICAOData (Point {pntLat = 47.06666666666667, pntLon = 2.3666666666666667, pntEle = Just 161.0, pntTime = Nothing}))
  , (LFLL, ICAOData (Point {pntLat = 45.733333333333334, pntLon = 5.083333333333333, pntEle = Just 248.0, pntTime = Nothing}))
  , (LFLM, ICAOData (Point {pntLat = 46.3, pntLon = 4.8, pntEle = Just 221.0, pntTime = Nothing}))
  , (LFLN, ICAOData (Point {pntLat = 46.416666666666664, pntLon = 4.016666666666667, pntEle = Just 242.0, pntTime = Nothing}))
  , (LFLQ, ICAOData (Point {pntLat = 44.583333333333336, pntLon = 4.733333333333333, pntEle = Just 73.0, pntTime = Nothing}))
  , (LFLS, ICAOData (Point {pntLat = 45.36666666666667, pntLon = 5.333333333333333, pntEle = Just 384.0, pntTime = Nothing}))
  , (LFLV, ICAOData (Point {pntLat = 46.166666666666664, pntLon = 3.4, pntEle = Just 249.0, pntTime = Nothing}))
  , (LFLW, ICAOData (Point {pntLat = 44.9, pntLon = 2.4166666666666665, pntEle = Just 640.0, pntTime = Nothing}))
  , (LFLX, ICAOData (Point {pntLat = 46.85, pntLon = 1.7166666666666668, pntEle = Just 160.0, pntTime = Nothing}))
  , (LFLY, ICAOData (Point {pntLat = 45.71666666666667, pntLon = 4.95, pntEle = Just 200.0, pntTime = Nothing}))
  , (LFMA, ICAOData (Point {pntLat = 43.5, pntLon = 5.366666666666666, pntEle = Just 111.0, pntTime = Nothing}))
  , (LFMC, ICAOData (Point {pntLat = 43.38333333333333, pntLon = 6.383333333333334, pntEle = Just 80.0, pntTime = Nothing}))
  , (LFMD, ICAOData (Point {pntLat = 43.55, pntLon = 6.95, pntEle = Just 3.0, pntTime = Nothing}))
  , (LFME, ICAOData (Point {pntLat = 43.86666666666667, pntLon = 4.4, pntEle = Just 60.0, pntTime = Nothing}))
  , (LFMH, ICAOData (Point {pntLat = 45.53333333333333, pntLon = 4.3, pntEle = Just 404.0, pntTime = Nothing}))
  , (LFMI, ICAOData (Point {pntLat = 43.516666666666666, pntLon = 4.933333333333334, pntEle = Just 23.0, pntTime = Nothing}))
  , (LFMK, ICAOData (Point {pntLat = 43.21666666666667, pntLon = 2.3166666666666664, pntEle = Just 130.0, pntTime = Nothing}))
  , (LFML, ICAOData (Point {pntLat = 43.45, pntLon = 5.233333333333333, pntEle = Just 6.0, pntTime = Nothing}))
  , (LFMN, ICAOData (Point {pntLat = 43.65, pntLon = 7.2, pntEle = Just 4.0, pntTime = Nothing}))
  , (LFMO, ICAOData (Point {pntLat = 44.13333333333333, pntLon = 4.833333333333333, pntEle = Just 60.0, pntTime = Nothing}))
  , (LFMP, ICAOData (Point {pntLat = 42.733333333333334, pntLon = 2.8666666666666667, pntEle = Just 43.0, pntTime = Nothing}))
  , (LFMT, ICAOData (Point {pntLat = 43.583333333333336, pntLon = 3.966666666666667, pntEle = Just 5.0, pntTime = Nothing}))
  , (LFMX, ICAOData (Point {pntLat = 44.06666666666667, pntLon = 6.0, pntEle = Just 460.0, pntTime = Nothing}))
  , (LFMY, ICAOData (Point {pntLat = 43.6, pntLon = 5.1, pntEle = Just 59.0, pntTime = Nothing}))
  , (LFOB, ICAOData (Point {pntLat = 49.46666666666667, pntLon = 2.1166666666666667, pntEle = Just 109.0, pntTime = Nothing}))
  , (LFOC, ICAOData (Point {pntLat = 48.05, pntLon = 1.3833333333333333, pntEle = Just 134.0, pntTime = Nothing}))
  , (LFOE, ICAOData (Point {pntLat = 49.016666666666666, pntLon = 1.2166666666666668, pntEle = Just 141.0, pntTime = Nothing}))
  , (LFOF, ICAOData (Point {pntLat = 48.45, pntLon = 0.11666666666666667, pntEle = Just 144.0, pntTime = Nothing}))
  , (LFOH, ICAOData (Point {pntLat = 49.516666666666666, pntLon = 6.666666666666667e-2, pntEle = Just 100.0, pntTime = Nothing}))
  , (LFOI, ICAOData (Point {pntLat = 50.13333333333333, pntLon = 1.8333333333333335, pntEle = Just 74.0, pntTime = Nothing}))
  , (LFOJ, ICAOData (Point {pntLat = 47.983333333333334, pntLon = 1.75, pntEle = Just 126.0, pntTime = Nothing}))
  , (LFOP, ICAOData (Point {pntLat = 49.38333333333333, pntLon = 1.1833333333333333, pntEle = Just 157.0, pntTime = Nothing}))
  , (LFOR, ICAOData (Point {pntLat = 48.46666666666667, pntLon = 1.5166666666666666, pntEle = Just 155.0, pntTime = Nothing}))
  , (LFOS, ICAOData (Point {pntLat = 49.833333333333336, pntLon = 0.65, pntEle = Just 83.0, pntTime = Nothing}))
  , (LFOT, ICAOData (Point {pntLat = 47.45, pntLon = 0.7166666666666667, pntEle = Just 108.0, pntTime = Nothing}))
  , (LFOW, ICAOData (Point {pntLat = 49.81666666666667, pntLon = 3.2, pntEle = Just 98.0, pntTime = Nothing}))
  , (LFPB, ICAOData (Point {pntLat = 48.96666666666667, pntLon = 2.45, pntEle = Just 66.0, pntTime = Nothing}))
  , (LFPC, ICAOData (Point {pntLat = 49.25, pntLon = 2.5166666666666666, pntEle = Just 88.0, pntTime = Nothing}))
  , (LFPG, ICAOData (Point {pntLat = 49.016666666666666, pntLon = 2.533333333333333, pntEle = Just 118.0, pntTime = Nothing}))
  , (LFPM, ICAOData (Point {pntLat = 48.61666666666667, pntLon = 2.6833333333333336, pntEle = Just 95.0, pntTime = Nothing}))
  , (LFPN, ICAOData (Point {pntLat = 48.766666666666666, pntLon = 1.1, pntEle = Just 164.0, pntTime = Nothing}))
  , (LFPO, ICAOData (Point {pntLat = 48.733333333333334, pntLon = 2.4, pntEle = Just 89.0, pntTime = Nothing}))
  , (LFPV, ICAOData (Point {pntLat = 48.766666666666666, pntLon = 2.2, pntEle = Just 177.0, pntTime = Nothing}))
  , (LFQB, ICAOData (Point {pntLat = 48.333333333333336, pntLon = 4.016666666666667, pntEle = Just 118.0, pntTime = Nothing}))
  , (LFQG, ICAOData (Point {pntLat = 47.0, pntLon = 3.1, pntEle = Just 180.0, pntTime = Nothing}))
  , (LFQH, ICAOData (Point {pntLat = 47.85, pntLon = 4.55, pntEle = Just 264.0, pntTime = Nothing}))
  , (LFQI, ICAOData (Point {pntLat = 50.21666666666667, pntLon = 3.15, pntEle = Just 78.0, pntTime = Nothing}))
  , (LFQQ, ICAOData (Point {pntLat = 50.56666666666667, pntLon = 3.1, pntEle = Just 48.0, pntTime = Nothing}))
  , (LFQV, ICAOData (Point {pntLat = 49.78333333333333, pntLon = 4.633333333333333, pntEle = Just 149.0, pntTime = Nothing}))
  , (LFRA, ICAOData (Point {pntLat = 47.5, pntLon = -0.5666666666666667, pntEle = Just 57.0, pntTime = Nothing}))
  , (LFRB, ICAOData (Point {pntLat = 48.45, pntLon = -4.416666666666667, pntEle = Just 99.0, pntTime = Nothing}))
  , (LFRC, ICAOData (Point {pntLat = 49.65, pntLon = -1.4666666666666668, pntEle = Just 138.0, pntTime = Nothing}))
  , (LFRD, ICAOData (Point {pntLat = 48.583333333333336, pntLon = -2.066666666666667, pntEle = Just 65.0, pntTime = Nothing}))
  , (LFRH, ICAOData (Point {pntLat = 47.766666666666666, pntLon = -3.45, pntEle = Just 52.0, pntTime = Nothing}))
  , (LFRI, ICAOData (Point {pntLat = 46.7, pntLon = -1.3833333333333333, pntEle = Just 90.0, pntTime = Nothing}))
  , (LFRJ, ICAOData (Point {pntLat = 48.516666666666666, pntLon = -4.15, pntEle = Just 106.0, pntTime = Nothing}))
  , (LFRK, ICAOData (Point {pntLat = 49.18333333333333, pntLon = -0.45, pntEle = Just 78.0, pntTime = Nothing}))
  , (LFRL, ICAOData (Point {pntLat = 48.28333333333333, pntLon = -4.433333333333334, pntEle = Just 87.0, pntTime = Nothing}))
  , (LFRM, ICAOData (Point {pntLat = 47.93333333333333, pntLon = 0.2, pntEle = Just 59.0, pntTime = Nothing}))
  , (LFRN, ICAOData (Point {pntLat = 48.06666666666667, pntLon = -1.7333333333333334, pntEle = Just 37.0, pntTime = Nothing}))
  , (LFRQ, ICAOData (Point {pntLat = 47.96666666666667, pntLon = -4.166666666666667, pntEle = Just 92.0, pntTime = Nothing}))
  , (LFRS, ICAOData (Point {pntLat = 47.166666666666664, pntLon = -1.6, pntEle = Just 27.0, pntTime = Nothing}))
  , (LFRT, ICAOData (Point {pntLat = 48.53333333333333, pntLon = -2.85, pntEle = Just 138.0, pntTime = Nothing}))
  , (LFRZ, ICAOData (Point {pntLat = 47.31666666666667, pntLon = -2.1666666666666665, pntEle = Just 3.0, pntTime = Nothing}))
  , (LFSA, ICAOData (Point {pntLat = 47.25, pntLon = 5.983333333333333, pntEle = Just 307.0, pntTime = Nothing}))
  , (LFSB, ICAOData (Point {pntLat = 47.6, pntLon = 7.516666666666667, pntEle = Just 270.0, pntTime = Nothing}))
  , (LFSC, ICAOData (Point {pntLat = 47.916666666666664, pntLon = 7.4, pntEle = Just 211.0, pntTime = Nothing}))
  , (LFSD, ICAOData (Point {pntLat = 47.266666666666666, pntLon = 5.083333333333333, pntEle = Just 222.0, pntTime = Nothing}))
  , (LFSF, ICAOData (Point {pntLat = 49.083333333333336, pntLon = 6.133333333333334, pntEle = Just 190.0, pntTime = Nothing}))
  , (LFSI, ICAOData (Point {pntLat = 48.63333333333333, pntLon = 4.9, pntEle = Just 139.0, pntTime = Nothing}))
  , (LFSL, ICAOData (Point {pntLat = 48.78333333333333, pntLon = 5.483333333333333, pntEle = Just 285.0, pntTime = Nothing}))
  , (LFSN, ICAOData (Point {pntLat = 48.68333333333333, pntLon = 6.216666666666667, pntEle = Just 225.0, pntTime = Nothing}))
  , (LFSO, ICAOData (Point {pntLat = 48.583333333333336, pntLon = 5.966666666666667, pntEle = Just 336.0, pntTime = Nothing}))
  , (LFSQ, ICAOData (Point {pntLat = 47.63333333333333, pntLon = 6.866666666666667, pntEle = Just 422.0, pntTime = Nothing}))
  , (LFSR, ICAOData (Point {pntLat = 49.3, pntLon = 4.033333333333333, pntEle = Just 95.0, pntTime = Nothing}))
  , (LFST, ICAOData (Point {pntLat = 48.55, pntLon = 7.633333333333333, pntEle = Just 153.0, pntTime = Nothing}))
  , (LFSX, ICAOData (Point {pntLat = 47.78333333333333, pntLon = 6.35, pntEle = Just 278.0, pntTime = Nothing}))
  , (LFTH, ICAOData (Point {pntLat = 43.1, pntLon = 6.15, pntEle = Just 3.0, pntTime = Nothing}))
  , (LFTU, ICAOData (Point {pntLat = 43.416666666666664, pntLon = 6.75, pntEle = Just 2.0, pntTime = Nothing}))
  , (LFTW, ICAOData (Point {pntLat = 43.75, pntLon = 4.416666666666667, pntEle = Just 94.0, pntTime = Nothing}))
  , (LFVP, ICAOData (Point {pntLat = 46.766666666666666, pntLon = -56.166666666666664, pntEle = Just 3.0, pntTime = Nothing}))
  , (LFXA, ICAOData (Point {pntLat = 45.983333333333334, pntLon = 5.333333333333333, pntEle = Just 250.0, pntTime = Nothing}))
  , (LFYR, ICAOData (Point {pntLat = 47.31666666666667, pntLon = 1.6833333333333333, pntEle = Just 85.0, pntTime = Nothing}))
  , (LGAD, ICAOData (Point {pntLat = 37.916666666666664, pntLon = 21.283333333333335, pntEle = Just 10.0, pntTime = Nothing}))
  , (LGAL, ICAOData (Point {pntLat = 40.85, pntLon = 25.916666666666668, pntEle = Just 7.0, pntTime = Nothing}))
  , (LGAT, ICAOData (Point {pntLat = 37.9, pntLon = 23.733333333333334, pntEle = Just 28.0, pntTime = Nothing}))
  , (LGBL, ICAOData (Point {pntLat = 39.21666666666667, pntLon = 22.8, pntEle = Just 25.0, pntTime = Nothing}))
  , (LGEL, ICAOData (Point {pntLat = 38.06666666666667, pntLon = 23.55, pntEle = Just 44.0, pntTime = Nothing}))
  , (LGHI, ICAOData (Point {pntLat = 38.333333333333336, pntLon = 26.133333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (LGIR, ICAOData (Point {pntLat = 35.333333333333336, pntLon = 25.183333333333334, pntEle = Just 37.0, pntTime = Nothing}))
  , (LGKA, ICAOData (Point {pntLat = 40.45, pntLon = 21.283333333333335, pntEle = Just 604.0, pntTime = Nothing}))
  , (LGKF, ICAOData (Point {pntLat = 38.11666666666667, pntLon = 20.5, pntEle = Just 19.0, pntTime = Nothing}))
  , (LGKL, ICAOData (Point {pntLat = 37.06666666666667, pntLon = 22.016666666666666, pntEle = Just 6.0, pntTime = Nothing}))
  , (LGKO, ICAOData (Point {pntLat = 36.78333333333333, pntLon = 27.066666666666666, pntEle = Just 129.0, pntTime = Nothing}))
  , (LGKP, ICAOData (Point {pntLat = 35.4, pntLon = 27.133333333333333, pntEle = Just 6.0, pntTime = Nothing}))
  , (LGKR, ICAOData (Point {pntLat = 39.61666666666667, pntLon = 19.916666666666668, pntEle = Just 2.0, pntTime = Nothing}))
  , (LGKV, ICAOData (Point {pntLat = 40.983333333333334, pntLon = 24.6, pntEle = Just 5.0, pntTime = Nothing}))
  , (LGKZ, ICAOData (Point {pntLat = 40.3, pntLon = 21.783333333333335, pntEle = Just 634.0, pntTime = Nothing}))
  , (LGLM, ICAOData (Point {pntLat = 39.916666666666664, pntLon = 25.233333333333334, pntEle = Just 5.0, pntTime = Nothing}))
  , (LGLR, ICAOData (Point {pntLat = 39.63333333333333, pntLon = 22.416666666666668, pntEle = Just 73.0, pntTime = Nothing}))
  , (LGMT, ICAOData (Point {pntLat = 39.06666666666667, pntLon = 26.6, pntEle = Just 3.0, pntTime = Nothing}))
  , (LGPZ, ICAOData (Point {pntLat = 38.61666666666667, pntLon = 20.766666666666666, pntEle = Just 4.0, pntTime = Nothing}))
  , (LGRP, ICAOData (Point {pntLat = 36.4, pntLon = 28.083333333333332, pntEle = Just 4.0, pntTime = Nothing}))
  , (LGRX, ICAOData (Point {pntLat = 38.15, pntLon = 21.416666666666668, pntEle = Just 11.0, pntTime = Nothing}))
  , (LGSA, ICAOData (Point {pntLat = 35.483333333333334, pntLon = 24.116666666666667, pntEle = Just 146.0, pntTime = Nothing}))
  , (LGSM, ICAOData (Point {pntLat = 37.7, pntLon = 26.916666666666668, pntEle = Just 2.0, pntTime = Nothing}))
  , (LGTG, ICAOData (Point {pntLat = 38.31666666666667, pntLon = 23.533333333333335, pntEle = Just 148.0, pntTime = Nothing}))
  , (LGTP, ICAOData (Point {pntLat = 37.53333333333333, pntLon = 22.4, pntEle = Just 644.0, pntTime = Nothing}))
  , (LGTS, ICAOData (Point {pntLat = 40.516666666666666, pntLon = 22.966666666666665, pntEle = Just 8.0, pntTime = Nothing}))
  , (LGTT, ICAOData (Point {pntLat = 38.1, pntLon = 23.783333333333335, pntEle = Just 239.0, pntTime = Nothing}))
  , (LHBC, ICAOData (Point {pntLat = 46.68333333333333, pntLon = 21.166666666666668, pntEle = Just 88.0, pntTime = Nothing}))
  , (LHBM, ICAOData (Point {pntLat = 47.516666666666666, pntLon = 19.033333333333335, pntEle = Just 129.0, pntTime = Nothing}))
  , (LHBP, ICAOData (Point {pntLat = 47.43333333333333, pntLon = 19.266666666666666, pntEle = Just 151.0, pntTime = Nothing}))
  , (LHBS, ICAOData (Point {pntLat = 47.45, pntLon = 18.966666666666665, pntEle = Just 132.0, pntTime = Nothing}))
  , (LHDC, ICAOData (Point {pntLat = 47.483333333333334, pntLon = 21.6, pntEle = Just 108.0, pntTime = Nothing}))
  , (LHKE, ICAOData (Point {pntLat = 46.916666666666664, pntLon = 19.75, pntEle = Just 113.0, pntTime = Nothing}))
  , (LHKV, ICAOData (Point {pntLat = 46.38333333333333, pntLon = 17.833333333333332, pntEle = Just 144.0, pntTime = Nothing}))
  , (LHMC, ICAOData (Point {pntLat = 48.083333333333336, pntLon = 20.766666666666666, pntEle = Just 232.0, pntTime = Nothing}))
  , (LHNY, ICAOData (Point {pntLat = 47.96666666666667, pntLon = 21.983333333333334, pntEle = Just 141.0, pntTime = Nothing}))
  , (LHPA, ICAOData (Point {pntLat = 47.2, pntLon = 17.5, pntEle = Just 145.0, pntTime = Nothing}))
  , (LHPP, ICAOData (Point {pntLat = 46.1, pntLon = 18.233333333333334, pntEle = Just 201.0, pntTime = Nothing}))
  , (LHSK, ICAOData (Point {pntLat = 46.916666666666664, pntLon = 18.05, pntEle = Just 108.0, pntTime = Nothing}))
  , (LHSN, ICAOData (Point {pntLat = 47.11666666666667, pntLon = 20.233333333333334, pntEle = Just 89.0, pntTime = Nothing}))
  , (LHSY, ICAOData (Point {pntLat = 47.266666666666666, pntLon = 16.633333333333333, pntEle = Just 220.0, pntTime = Nothing}))
  , (LHUD, ICAOData (Point {pntLat = 46.25, pntLon = 20.1, pntEle = Just 82.0, pntTime = Nothing}))
  , (LIBA, ICAOData (Point {pntLat = 41.53333333333333, pntLon = 15.716666666666667, pntEle = Just 57.0, pntTime = Nothing}))
  , (LIBC, ICAOData (Point {pntLat = 39.0, pntLon = 17.066666666666666, pntEle = Just 155.0, pntTime = Nothing}))
  , (LIBD, ICAOData (Point {pntLat = 41.13333333333333, pntLon = 16.783333333333335, pntEle = Just 34.0, pntTime = Nothing}))
  , (LIBE, ICAOData (Point {pntLat = 41.7, pntLon = 15.95, pntEle = Just 838.0, pntTime = Nothing}))
  , (LIBG, ICAOData (Point {pntLat = 40.516666666666666, pntLon = 17.4, pntEle = Just 64.0, pntTime = Nothing}))
  , (LIBH, ICAOData (Point {pntLat = 40.43333333333333, pntLon = 16.883333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (LIBN, ICAOData (Point {pntLat = 40.233333333333334, pntLon = 18.15, pntEle = Just 48.0, pntTime = Nothing}))
  , (LIBP, ICAOData (Point {pntLat = 42.43333333333333, pntLon = 14.2, pntEle = Just 10.0, pntTime = Nothing}))
  , (LIBQ, ICAOData (Point {pntLat = 39.333333333333336, pntLon = 16.4, pntEle = Just 1710.0, pntTime = Nothing}))
  , (LIBR, ICAOData (Point {pntLat = 40.65, pntLon = 17.95, pntEle = Just 15.0, pntTime = Nothing}))
  , (LIBS, ICAOData (Point {pntLat = 41.56666666666667, pntLon = 14.65, pntEle = Just 793.0, pntTime = Nothing}))
  , (LIBT, ICAOData (Point {pntLat = 42.0, pntLon = 15.0, pntEle = Just 16.0, pntTime = Nothing}))
  , (LIBU, ICAOData (Point {pntLat = 40.083333333333336, pntLon = 16.016666666666666, pntEle = Just 888.0, pntTime = Nothing}))
  , (LIBV, ICAOData (Point {pntLat = 40.766666666666666, pntLon = 16.933333333333334, pntEle = Just 345.0, pntTime = Nothing}))
  , (LIBW, ICAOData (Point {pntLat = 39.583333333333336, pntLon = 15.883333333333333, pntEle = Just 484.0, pntTime = Nothing}))
  , (LIBY, ICAOData (Point {pntLat = 39.81666666666667, pntLon = 18.35, pntEle = Just 104.0, pntTime = Nothing}))
  , (LIBZ, ICAOData (Point {pntLat = 40.63333333333333, pntLon = 15.8, pntEle = Just 823.0, pntTime = Nothing}))
  , (LICA, ICAOData (Point {pntLat = 38.9, pntLon = 16.25, pntEle = Just 15.0, pntTime = Nothing}))
  , (LICC, ICAOData (Point {pntLat = 37.46666666666667, pntLon = 15.05, pntEle = Just 11.0, pntTime = Nothing}))
  , (LICD, ICAOData (Point {pntLat = 35.5, pntLon = 12.6, pntEle = Just 16.0, pntTime = Nothing}))
  , (LICE, ICAOData (Point {pntLat = 37.56666666666667, pntLon = 14.283333333333333, pntEle = Just 940.0, pntTime = Nothing}))
  , (LICF, ICAOData (Point {pntLat = 38.2, pntLon = 15.55, pntEle = Just 59.0, pntTime = Nothing}))
  , (LICG, ICAOData (Point {pntLat = 36.81666666666667, pntLon = 11.966666666666667, pntEle = Just 191.0, pntTime = Nothing}))
  , (LICJ, ICAOData (Point {pntLat = 38.18333333333333, pntLon = 13.1, pntEle = Just 21.0, pntTime = Nothing}))
  , (LICL, ICAOData (Point {pntLat = 37.083333333333336, pntLon = 14.216666666666667, pntEle = Just 11.0, pntTime = Nothing}))
  , (LICO, ICAOData (Point {pntLat = 36.68333333333333, pntLon = 15.133333333333333, pntEle = Just 46.0, pntTime = Nothing}))
  , (LICP, ICAOData (Point {pntLat = 38.1, pntLon = 13.3, pntEle = Just 107.0, pntTime = Nothing}))
  , (LICR, ICAOData (Point {pntLat = 38.06666666666667, pntLon = 15.65, pntEle = Just 11.0, pntTime = Nothing}))
  , (LICT, ICAOData (Point {pntLat = 37.916666666666664, pntLon = 12.5, pntEle = Just 7.0, pntTime = Nothing}))
  , (LICU, ICAOData (Point {pntLat = 38.7, pntLon = 13.183333333333334, pntEle = Just 250.0, pntTime = Nothing}))
  , (LICZ, ICAOData (Point {pntLat = 37.4, pntLon = 14.916666666666666, pntEle = Just 22.0, pntTime = Nothing}))
  , (LIEA, ICAOData (Point {pntLat = 40.63333333333333, pntLon = 8.283333333333333, pntEle = Just 23.0, pntTime = Nothing}))
  , (LIEB, ICAOData (Point {pntLat = 39.93333333333333, pntLon = 9.716666666666667, pntEle = Just 138.0, pntTime = Nothing}))
  , (LIEC, ICAOData (Point {pntLat = 39.1, pntLon = 9.516666666666667, pntEle = Just 116.0, pntTime = Nothing}))
  , (LIED, ICAOData (Point {pntLat = 39.35, pntLon = 8.966666666666667, pntEle = Just 29.0, pntTime = Nothing}))
  , (LIEE, ICAOData (Point {pntLat = 39.25, pntLon = 9.066666666666666, pntEle = Just 4.0, pntTime = Nothing}))
  , (LIEF, ICAOData (Point {pntLat = 39.75, pntLon = 8.466666666666667, pntEle = Just 89.0, pntTime = Nothing}))
  , (LIEG, ICAOData (Point {pntLat = 41.21666666666667, pntLon = 9.4, pntEle = Just 158.0, pntTime = Nothing}))
  , (LIEH, ICAOData (Point {pntLat = 40.56666666666667, pntLon = 8.166666666666666, pntEle = Just 200.0, pntTime = Nothing}))
  , (LIEL, ICAOData (Point {pntLat = 39.5, pntLon = 9.633333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (LIEN, ICAOData (Point {pntLat = 40.11666666666667, pntLon = 9.25, pntEle = Just 1022.0, pntTime = Nothing}))
  , (LIEO, ICAOData (Point {pntLat = 40.9, pntLon = 9.516666666666667, pntEle = Just 11.0, pntTime = Nothing}))
  , (LIEP, ICAOData (Point {pntLat = 39.666666666666664, pntLon = 9.433333333333334, pntEle = Just 608.0, pntTime = Nothing}))
  , (LIMC, ICAOData (Point {pntLat = 45.61666666666667, pntLon = 8.733333333333333, pntEle = Just 234.0, pntTime = Nothing}))
  , (LIME, ICAOData (Point {pntLat = 45.666666666666664, pntLon = 9.7, pntEle = Just 238.0, pntTime = Nothing}))
  , (LIMF, ICAOData (Point {pntLat = 45.21666666666667, pntLon = 7.65, pntEle = Just 301.0, pntTime = Nothing}))
  , (LIMG, ICAOData (Point {pntLat = 44.05, pntLon = 8.116666666666667, pntEle = Just 45.0, pntTime = Nothing}))
  , (LIMH, ICAOData (Point {pntLat = 45.93333333333333, pntLon = 7.7, pntEle = Just 3480.0, pntTime = Nothing}))
  , (LIMJ, ICAOData (Point {pntLat = 44.416666666666664, pntLon = 8.85, pntEle = Just 2.0, pntTime = Nothing}))
  , (LIMK, ICAOData (Point {pntLat = 45.03333333333333, pntLon = 7.733333333333333, pntEle = Just 709.0, pntTime = Nothing}))
  , (LIML, ICAOData (Point {pntLat = 45.43333333333333, pntLon = 9.283333333333333, pntEle = Just 107.0, pntTime = Nothing}))
  , (LIMN, ICAOData (Point {pntLat = 45.516666666666666, pntLon = 8.666666666666666, pntEle = Just 178.0, pntTime = Nothing}))
  , (LIMO, ICAOData (Point {pntLat = 45.86666666666667, pntLon = 9.066666666666666, pntEle = Just 1319.0, pntTime = Nothing}))
  , (LIMS, ICAOData (Point {pntLat = 44.916666666666664, pntLon = 9.733333333333333, pntEle = Just 134.0, pntTime = Nothing}))
  , (LIMT, ICAOData (Point {pntLat = 44.43333333333333, pntLon = 9.933333333333334, pntEle = Just 1039.0, pntTime = Nothing}))
  , (LIMU, ICAOData (Point {pntLat = 43.95, pntLon = 8.166666666666666, pntEle = Just 220.0, pntTime = Nothing}))
  , (LIMV, ICAOData (Point {pntLat = 44.63333333333333, pntLon = 8.933333333333334, pntEle = Just 468.0, pntTime = Nothing}))
  , (LIMY, ICAOData (Point {pntLat = 44.25, pntLon = 7.8, pntEle = Just 1741.0, pntTime = Nothing}))
  , (LIPA, ICAOData (Point {pntLat = 46.03333333333333, pntLon = 12.6, pntEle = Just 128.0, pntTime = Nothing}))
  , (LIPB, ICAOData (Point {pntLat = 46.46666666666667, pntLon = 11.333333333333334, pntEle = Just 1459.0, pntTime = Nothing}))
  , (LIPC, ICAOData (Point {pntLat = 44.21666666666667, pntLon = 12.3, pntEle = Just 6.0, pntTime = Nothing}))
  , (LIPD, ICAOData (Point {pntLat = 46.03333333333333, pntLon = 13.183333333333334, pntEle = Just 93.0, pntTime = Nothing}))
  , (LIPE, ICAOData (Point {pntLat = 44.53333333333333, pntLon = 11.3, pntEle = Just 36.0, pntTime = Nothing}))
  , (LIPF, ICAOData (Point {pntLat = 44.833333333333336, pntLon = 11.616666666666667, pntEle = Just 10.0, pntTime = Nothing}))
  , (LIPH, ICAOData (Point {pntLat = 45.65, pntLon = 12.183333333333334, pntEle = Just 18.0, pntTime = Nothing}))
  , (LIPI, ICAOData (Point {pntLat = 45.983333333333334, pntLon = 13.033333333333333, pntEle = Just 51.0, pntTime = Nothing}))
  , (LIPK, ICAOData (Point {pntLat = 44.2, pntLon = 12.066666666666666, pntEle = Just 27.0, pntTime = Nothing}))
  , (LIPL, ICAOData (Point {pntLat = 45.416666666666664, pntLon = 10.283333333333333, pntEle = Just 102.0, pntTime = Nothing}))
  , (LIPQ, ICAOData (Point {pntLat = 45.81666666666667, pntLon = 13.483333333333333, pntEle = Just 12.0, pntTime = Nothing}))
  , (LIPR, ICAOData (Point {pntLat = 44.03333333333333, pntLon = 12.616666666666667, pntEle = Just 12.0, pntTime = Nothing}))
  , (LIPS, ICAOData (Point {pntLat = 45.68333333333333, pntLon = 12.1, pntEle = Just 45.0, pntTime = Nothing}))
  , (LIPT, ICAOData (Point {pntLat = 45.56666666666667, pntLon = 11.516666666666667, pntEle = Just 39.0, pntTime = Nothing}))
  , (LIPX, ICAOData (Point {pntLat = 45.38333333333333, pntLon = 10.866666666666667, pntEle = Just 67.0, pntTime = Nothing}))
  , (LIPY, ICAOData (Point {pntLat = 43.61666666666667, pntLon = 13.366666666666667, pntEle = Just 12.0, pntTime = Nothing}))
  , (LIPZ, ICAOData (Point {pntLat = 45.5, pntLon = 12.333333333333334, pntEle = Just 2.0, pntTime = Nothing}))
  , (LIQB, ICAOData (Point {pntLat = 43.46666666666667, pntLon = 11.85, pntEle = Just 248.0, pntTime = Nothing}))
  , (LIQC, ICAOData (Point {pntLat = 40.55, pntLon = 14.2, pntEle = Just 160.0, pntTime = Nothing}))
  , (LIQJ, ICAOData (Point {pntLat = 42.03333333333333, pntLon = 11.833333333333334, pntEle = Just 3.0, pntTime = Nothing}))
  , (LIQK, ICAOData (Point {pntLat = 40.016666666666666, pntLon = 15.283333333333333, pntEle = Just 184.0, pntTime = Nothing}))
  , (LIQN, ICAOData (Point {pntLat = 42.416666666666664, pntLon = 12.85, pntEle = Just 389.0, pntTime = Nothing}))
  , (LIQO, ICAOData (Point {pntLat = 42.38333333333333, pntLon = 11.166666666666666, pntEle = Just 630.0, pntTime = Nothing}))
  , (LIQP, ICAOData (Point {pntLat = 44.03333333333333, pntLon = 9.833333333333334, pntEle = Just 192.0, pntTime = Nothing}))
  , (LIQR, ICAOData (Point {pntLat = 42.9, pntLon = 11.766666666666667, pntEle = Just 816.0, pntTime = Nothing}))
  , (LIQV, ICAOData (Point {pntLat = 43.4, pntLon = 10.866666666666667, pntEle = Just 555.0, pntTime = Nothing}))
  , (LIQW, ICAOData (Point {pntLat = 44.083333333333336, pntLon = 9.983333333333333, pntEle = Just 9.0, pntTime = Nothing}))
  , (LIQZ, ICAOData (Point {pntLat = 40.916666666666664, pntLon = 12.95, pntEle = Just 184.0, pntTime = Nothing}))
  , (LIRA, ICAOData (Point {pntLat = 41.78333333333333, pntLon = 12.583333333333334, pntEle = Just 129.0, pntTime = Nothing}))
  , (LIRB, ICAOData (Point {pntLat = 42.083333333333336, pntLon = 12.216666666666667, pntEle = Just 262.0, pntTime = Nothing}))
  , (LIRE, ICAOData (Point {pntLat = 41.65, pntLon = 12.45, pntEle = Just 6.0, pntTime = Nothing}))
  , (LIRF, ICAOData (Point {pntLat = 41.8, pntLon = 12.233333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (LIRG, ICAOData (Point {pntLat = 42.0, pntLon = 12.733333333333333, pntEle = Just 88.0, pntTime = Nothing}))
  , (LIRH, ICAOData (Point {pntLat = 41.63333333333333, pntLon = 13.3, pntEle = Just 180.0, pntTime = Nothing}))
  , (LIRJ, ICAOData (Point {pntLat = 42.733333333333334, pntLon = 10.4, pntEle = Just 396.0, pntTime = Nothing}))
  , (LIRK, ICAOData (Point {pntLat = 42.46666666666667, pntLon = 12.983333333333333, pntEle = Just 1874.0, pntTime = Nothing}))
  , (LIRL, ICAOData (Point {pntLat = 41.55, pntLon = 12.9, pntEle = Just 25.0, pntTime = Nothing}))
  , (LIRM, ICAOData (Point {pntLat = 41.05, pntLon = 14.066666666666666, pntEle = Just 9.0, pntTime = Nothing}))
  , (LIRN, ICAOData (Point {pntLat = 40.85, pntLon = 14.3, pntEle = Just 88.0, pntTime = Nothing}))
  , (LIRP, ICAOData (Point {pntLat = 43.68333333333333, pntLon = 10.383333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (LIRQ, ICAOData (Point {pntLat = 43.8, pntLon = 11.2, pntEle = Just 40.0, pntTime = Nothing}))
  , (LIRS, ICAOData (Point {pntLat = 42.75, pntLon = 11.066666666666666, pntEle = Just 5.0, pntTime = Nothing}))
  , (LIRT, ICAOData (Point {pntLat = 41.05, pntLon = 15.233333333333333, pntEle = Just 1085.0, pntTime = Nothing}))
  , (LIRU, ICAOData (Point {pntLat = 41.95, pntLon = 12.5, pntEle = Just 18.0, pntTime = Nothing}))
  , (LIRV, ICAOData (Point {pntLat = 42.43333333333333, pntLon = 12.05, pntEle = Just 300.0, pntTime = Nothing}))
  , (LIRZ, ICAOData (Point {pntLat = 43.083333333333336, pntLon = 12.5, pntEle = Just 208.0, pntTime = Nothing}))
  , (LIVC, ICAOData (Point {pntLat = 44.2, pntLon = 10.7, pntEle = Just 2165.0, pntTime = Nothing}))
  , (LIVD, ICAOData (Point {pntLat = 46.733333333333334, pntLon = 12.216666666666667, pntEle = Just 1222.0, pntTime = Nothing}))
  , (LIVF, ICAOData (Point {pntLat = 43.516666666666666, pntLon = 12.733333333333333, pntEle = Just 570.0, pntTime = Nothing}))
  , (LIVM, ICAOData (Point {pntLat = 44.45, pntLon = 12.3, pntEle = Just 2.0, pntTime = Nothing}))
  , (LIVO, ICAOData (Point {pntLat = 46.5, pntLon = 13.583333333333334, pntEle = Just 777.0, pntTime = Nothing}))
  , (LIVP, ICAOData (Point {pntLat = 46.15, pntLon = 11.033333333333333, pntEle = Just 2125.0, pntTime = Nothing}))
  , (LIVR, ICAOData (Point {pntLat = 46.3, pntLon = 11.783333333333333, pntEle = Just 2004.0, pntTime = Nothing}))
  , (LIVT, ICAOData (Point {pntLat = 45.65, pntLon = 13.75, pntEle = Just 8.0, pntTime = Nothing}))
  , (LIYW, ICAOData (Point {pntLat = 46.03333333333333, pntLon = 12.616666666666667, pntEle = Just 126.0, pntTime = Nothing}))
  , (LJLJ, ICAOData (Point {pntLat = 46.21666666666667, pntLon = 14.483333333333333, pntEle = Just 385.0, pntTime = Nothing}))
  , (LJMB, ICAOData (Point {pntLat = 46.483333333333334, pntLon = 15.683333333333334, pntEle = Just 264.0, pntTime = Nothing}))
  , (LJMS, ICAOData (Point {pntLat = 46.65, pntLon = 16.183333333333334, pntEle = Just 188.0, pntTime = Nothing}))
  , (LJPZ, ICAOData (Point {pntLat = 45.516666666666666, pntLon = 13.566666666666666, pntEle = Just 95.0, pntTime = Nothing}))
  , (LKHO, ICAOData (Point {pntLat = 49.31666666666667, pntLon = 17.566666666666666, pntEle = Just 224.0, pntTime = Nothing}))
  , (LKKV, ICAOData (Point {pntLat = 50.2, pntLon = 12.916666666666666, pntEle = Just 603.0, pntTime = Nothing}))
  , (LKKZ, ICAOData (Point {pntLat = 48.666666666666664, pntLon = 21.216666666666665, pntEle = Just 230.0, pntTime = Nothing}))
  , (LKMT, ICAOData (Point {pntLat = 49.68333333333333, pntLon = 18.116666666666667, pntEle = Just 256.0, pntTime = Nothing}))
  , (LKPP, ICAOData (Point {pntLat = 48.61666666666667, pntLon = 17.833333333333332, pntEle = Just 163.0, pntTime = Nothing}))
  , (LKPR, ICAOData (Point {pntLat = 50.1, pntLon = 14.25, pntEle = Just 365.0, pntTime = Nothing}))
  , (LKSL, ICAOData (Point {pntLat = 48.65, pntLon = 19.15, pntEle = Just 314.0, pntTime = Nothing}))
  , (LKTB, ICAOData (Point {pntLat = 49.15, pntLon = 16.7, pntEle = Just 241.0, pntTime = Nothing}))
  , (LLBG, ICAOData (Point {pntLat = 32.0, pntLon = 34.9, pntEle = Just 40.0, pntTime = Nothing}))
  , (LLBS, ICAOData (Point {pntLat = 31.233333333333334, pntLon = 34.78333333333333, pntEle = Just 275.0, pntTime = Nothing}))
  , (LLET, ICAOData (Point {pntLat = 29.55, pntLon = 34.95, pntEle = Just 12.0, pntTime = Nothing}))
  , (LLHA, ICAOData (Point {pntLat = 32.8, pntLon = 35.03333333333333, pntEle = Just 8.0, pntTime = Nothing}))
  , (LLJR, ICAOData (Point {pntLat = 31.866666666666667, pntLon = 35.21666666666667, pntEle = Just 749.0, pntTime = Nothing}))
  , (LLOV, ICAOData (Point {pntLat = 30.0, pntLon = 34.833333333333336, pntEle = Just 432.0, pntTime = Nothing}))
  , (LMML, ICAOData (Point {pntLat = 35.85, pntLon = 14.483333333333333, pntEle = Just 91.0, pntTime = Nothing}))
  , (LOAV, ICAOData (Point {pntLat = 47.95, pntLon = 16.25, pntEle = Just 233.0, pntTime = Nothing}))
  , (LOWG, ICAOData (Point {pntLat = 47.0, pntLon = 15.433333333333334, pntEle = Just 340.0, pntTime = Nothing}))
  , (LOWI, ICAOData (Point {pntLat = 47.266666666666666, pntLon = 11.35, pntEle = Just 581.0, pntTime = Nothing}))
  , (LOWK, ICAOData (Point {pntLat = 46.65, pntLon = 14.333333333333334, pntEle = Just 448.0, pntTime = Nothing}))
  , (LOWL, ICAOData (Point {pntLat = 48.233333333333334, pntLon = 14.183333333333334, pntEle = Just 298.0, pntTime = Nothing}))
  , (LOWS, ICAOData (Point {pntLat = 47.8, pntLon = 13.0, pntEle = Just 430.0, pntTime = Nothing}))
  , (LOWW, ICAOData (Point {pntLat = 48.11666666666667, pntLon = 16.566666666666666, pntEle = Just 183.0, pntTime = Nothing}))
  , (LOXA, ICAOData (Point {pntLat = 47.53333333333333, pntLon = 14.133333333333333, pntEle = Just 638.0, pntTime = Nothing}))
  , (LOXL, ICAOData (Point {pntLat = 48.233333333333334, pntLon = 14.183333333333334, pntEle = Just 313.0, pntTime = Nothing}))
  , (LOXS, ICAOData (Point {pntLat = 47.333333333333336, pntLon = 11.7, pntEle = Just 543.0, pntTime = Nothing}))
  , (LOXT, ICAOData (Point {pntLat = 48.31666666666667, pntLon = 16.116666666666667, pntEle = Just 175.0, pntTime = Nothing}))
  , (LOXZ, ICAOData (Point {pntLat = 47.2, pntLon = 14.75, pntEle = Just 677.0, pntTime = Nothing}))
  , (LPAZ, ICAOData (Point {pntLat = 36.96666666666667, pntLon = -25.166666666666668, pntEle = Just 100.0, pntTime = Nothing}))
  , (LPBG, ICAOData (Point {pntLat = 41.8, pntLon = -6.733333333333333, pntEle = Just 691.0, pntTime = Nothing}))
  , (LPBJ, ICAOData (Point {pntLat = 38.016666666666666, pntLon = -7.866666666666667, pntEle = Just 246.0, pntTime = Nothing}))
  , (LPFL, ICAOData (Point {pntLat = 39.45, pntLon = -31.133333333333333, pntEle = Just 28.0, pntTime = Nothing}))
  , (LPFR, ICAOData (Point {pntLat = 37.016666666666666, pntLon = -7.966666666666667, pntEle = Just 7.0, pntTime = Nothing}))
  , (LPFU, ICAOData (Point {pntLat = 32.68333333333333, pntLon = -16.766666666666666, pntEle = Just 58.0, pntTime = Nothing}))
  , (LPHR, ICAOData (Point {pntLat = 38.516666666666666, pntLon = -28.716666666666665, pntEle = Just 40.0, pntTime = Nothing}))
  , (LPLA, ICAOData (Point {pntLat = 38.766666666666666, pntLon = -27.1, pntEle = Just 52.0, pntTime = Nothing}))
  , (LPPD, ICAOData (Point {pntLat = 37.733333333333334, pntLon = -25.7, pntEle = Just 71.0, pntTime = Nothing}))
  , (LPPR, ICAOData (Point {pntLat = 41.233333333333334, pntLon = -8.683333333333334, pntEle = Just 69.0, pntTime = Nothing}))
  , (LPPS, ICAOData (Point {pntLat = 33.06666666666667, pntLon = -16.35, pntEle = Just 97.0, pntTime = Nothing}))
  , (LPPT, ICAOData (Point {pntLat = 38.766666666666666, pntLon = -9.133333333333333, pntEle = Just 114.0, pntTime = Nothing}))
  , (LPVR, ICAOData (Point {pntLat = 41.266666666666666, pntLon = -7.716666666666667, pntEle = Just 561.0, pntTime = Nothing}))
  , (LQBI, ICAOData (Point {pntLat = 44.81666666666667, pntLon = 15.883333333333333, pntEle = Just 250.0, pntTime = Nothing}))
  , (LQBK, ICAOData (Point {pntLat = 44.78333333333333, pntLon = 17.216666666666665, pntEle = Just 156.0, pntTime = Nothing}))
  , (LQLV, ICAOData (Point {pntLat = 43.833333333333336, pntLon = 17.016666666666666, pntEle = Just 724.0, pntTime = Nothing}))
  , (LQMO, ICAOData (Point {pntLat = 43.35, pntLon = 17.8, pntEle = Just 108.0, pntTime = Nothing}))
  , (LQSA, ICAOData (Point {pntLat = 43.81666666666667, pntLon = 18.333333333333332, pntEle = Just 511.0, pntTime = Nothing}))
  , (LQTZ, ICAOData (Point {pntLat = 44.55, pntLon = 18.7, pntEle = Just 306.0, pntTime = Nothing}))
  , (LRAR, ICAOData (Point {pntLat = 46.2, pntLon = 21.4, pntEle = Just 117.0, pntTime = Nothing}))
  , (LRBC, ICAOData (Point {pntLat = 46.583333333333336, pntLon = 26.966666666666665, pntEle = Just 184.0, pntTime = Nothing}))
  , (LRBM, ICAOData (Point {pntLat = 47.666666666666664, pntLon = 23.583333333333332, pntEle = Just 218.0, pntTime = Nothing}))
  , (LRBS, ICAOData (Point {pntLat = 44.5, pntLon = 26.133333333333333, pntEle = Just 90.0, pntTime = Nothing}))
  , (LRCK, ICAOData (Point {pntLat = 44.333333333333336, pntLon = 28.433333333333334, pntEle = Just 97.0, pntTime = Nothing}))
  , (LRCL, ICAOData (Point {pntLat = 46.78333333333333, pntLon = 23.566666666666666, pntEle = Just 410.0, pntTime = Nothing}))
  , (LRCS, ICAOData (Point {pntLat = 45.416666666666664, pntLon = 22.25, pntEle = Just 241.0, pntTime = Nothing}))
  , (LRCV, ICAOData (Point {pntLat = 44.233333333333334, pntLon = 23.866666666666667, pntEle = Just 192.0, pntTime = Nothing}))
  , (LRIA, ICAOData (Point {pntLat = 47.166666666666664, pntLon = 27.633333333333333, pntEle = Just 102.0, pntTime = Nothing}))
  , (LROD, ICAOData (Point {pntLat = 47.05, pntLon = 21.933333333333334, pntEle = Just 135.0, pntTime = Nothing}))
  , (LROP, ICAOData (Point {pntLat = 44.55, pntLon = 26.1, pntEle = Just 95.0, pntTime = Nothing}))
  , (LRSB, ICAOData (Point {pntLat = 45.8, pntLon = 24.15, pntEle = Just 443.0, pntTime = Nothing}))
  , (LRSM, ICAOData (Point {pntLat = 47.8, pntLon = 22.883333333333333, pntEle = Just 123.0, pntTime = Nothing}))
  , (LRSV, ICAOData (Point {pntLat = 47.65, pntLon = 26.25, pntEle = Just 351.0, pntTime = Nothing}))
  , (LRTC, ICAOData (Point {pntLat = 45.18333333333333, pntLon = 28.816666666666666, pntEle = Just 4.0, pntTime = Nothing}))
  , (LRTM, ICAOData (Point {pntLat = 46.53333333333333, pntLon = 24.533333333333335, pntEle = Just 308.0, pntTime = Nothing}))
  , (LRTR, ICAOData (Point {pntLat = 45.766666666666666, pntLon = 21.25, pntEle = Just 86.0, pntTime = Nothing}))
  , (LSGG, ICAOData (Point {pntLat = 46.25, pntLon = 6.133333333333334, pntEle = Just 420.0, pntTime = Nothing}))
  , (LSGL, ICAOData (Point {pntLat = 46.55, pntLon = 6.616666666666667, pntEle = Just 616.0, pntTime = Nothing}))
  , (LSGN, ICAOData (Point {pntLat = 47.0, pntLon = 6.95, pntEle = Just 485.0, pntTime = Nothing}))
  , (LSGS, ICAOData (Point {pntLat = 46.21666666666667, pntLon = 7.333333333333333, pntEle = Just 482.0, pntTime = Nothing}))
  , (LSMP, ICAOData (Point {pntLat = 46.81666666666667, pntLon = 6.95, pntEle = Just 490.0, pntTime = Nothing}))
  , (LSZA, ICAOData (Point {pntLat = 46.0, pntLon = 8.966666666666667, pntEle = Just 273.0, pntTime = Nothing}))
  , (LSZG, ICAOData (Point {pntLat = 47.166666666666664, pntLon = 7.4, pntEle = Just 430.0, pntTime = Nothing}))
  , (LSZH, ICAOData (Point {pntLat = 47.483333333333334, pntLon = 8.533333333333333, pntEle = Just 436.0, pntTime = Nothing}))
  , (LSZR, ICAOData (Point {pntLat = 47.483333333333334, pntLon = 9.55, pntEle = Just 398.0, pntTime = Nothing}))
  , (LTAC, ICAOData (Point {pntLat = 40.11666666666667, pntLon = 32.983333333333334, pntEle = Just 953.0, pntTime = Nothing}))
  , (LTAD, ICAOData (Point {pntLat = 39.95, pntLon = 32.68333333333333, pntEle = Just 799.0, pntTime = Nothing}))
  , (LTAE, ICAOData (Point {pntLat = 40.083333333333336, pntLon = 32.56666666666667, pntEle = Just 843.0, pntTime = Nothing}))
  , (LTAF, ICAOData (Point {pntLat = 36.983333333333334, pntLon = 35.3, pntEle = Just 20.0, pntTime = Nothing}))
  , (LTAG, ICAOData (Point {pntLat = 37.0, pntLon = 35.416666666666664, pntEle = Just 73.0, pntTime = Nothing}))
  , (LTAH, ICAOData (Point {pntLat = 38.75, pntLon = 30.533333333333335, pntEle = Just 1034.0, pntTime = Nothing}))
  , (LTAI, ICAOData (Point {pntLat = 36.7, pntLon = 30.733333333333334, pntEle = Just 50.0, pntTime = Nothing}))
  , (LTAJ, ICAOData (Point {pntLat = 37.083333333333336, pntLon = 37.36666666666667, pntEle = Just 701.0, pntTime = Nothing}))
  , (LTAK, ICAOData (Point {pntLat = 36.583333333333336, pntLon = 36.166666666666664, pntEle = Just 3.0, pntTime = Nothing}))
  , (LTAN, ICAOData (Point {pntLat = 37.96666666666667, pntLon = 32.55, pntEle = Just 1032.0, pntTime = Nothing}))
  , (LTAP, ICAOData (Point {pntLat = 40.85, pntLon = 35.583333333333336, pntEle = Just 545.0, pntTime = Nothing}))
  , (LTAQ, ICAOData (Point {pntLat = 41.266666666666666, pntLon = 36.3, pntEle = Just 162.0, pntTime = Nothing}))
  , (LTAR, ICAOData (Point {pntLat = 39.75, pntLon = 37.016666666666666, pntEle = Just 1285.0, pntTime = Nothing}))
  , (LTAS, ICAOData (Point {pntLat = 41.45, pntLon = 31.8, pntEle = Just 136.0, pntTime = Nothing}))
  , (LTAT, ICAOData (Point {pntLat = 38.43333333333333, pntLon = 38.083333333333336, pntEle = Just 862.0, pntTime = Nothing}))
  , (LTAU, ICAOData (Point {pntLat = 38.78333333333333, pntLon = 35.483333333333334, pntEle = Just 1053.0, pntTime = Nothing}))
  , (LTAW, ICAOData (Point {pntLat = 40.3, pntLon = 36.56666666666667, pntEle = Just 608.0, pntTime = Nothing}))
  , (LTBA, ICAOData (Point {pntLat = 40.96666666666667, pntLon = 28.816666666666666, pntEle = Just 48.0, pntTime = Nothing}))
  , (LTBD, ICAOData (Point {pntLat = 37.85, pntLon = 27.85, pntEle = Just 57.0, pntTime = Nothing}))
  , (LTBE, ICAOData (Point {pntLat = 40.18333333333333, pntLon = 29.066666666666666, pntEle = Just 101.0, pntTime = Nothing}))
  , (LTBF, ICAOData (Point {pntLat = 39.61666666666667, pntLon = 27.916666666666668, pntEle = Just 101.0, pntTime = Nothing}))
  , (LTBG, ICAOData (Point {pntLat = 40.31666666666667, pntLon = 27.966666666666665, pntEle = Just 51.0, pntTime = Nothing}))
  , (LTBH, ICAOData (Point {pntLat = 40.13333333333333, pntLon = 26.4, pntEle = Just 3.0, pntTime = Nothing}))
  , (LTBI, ICAOData (Point {pntLat = 39.78333333333333, pntLon = 30.566666666666666, pntEle = Just 785.0, pntTime = Nothing}))
  , (LTBJ, ICAOData (Point {pntLat = 38.266666666666666, pntLon = 27.15, pntEle = Just 125.0, pntTime = Nothing}))
  , (LTBL, ICAOData (Point {pntLat = 38.5, pntLon = 27.016666666666666, pntEle = Just 5.0, pntTime = Nothing}))
  , (LTBM, ICAOData (Point {pntLat = 37.75, pntLon = 30.55, pntEle = Just 997.0, pntTime = Nothing}))
  , (LTBO, ICAOData (Point {pntLat = 38.666666666666664, pntLon = 29.416666666666668, pntEle = Just 919.0, pntTime = Nothing}))
  , (LTBS, ICAOData (Point {pntLat = 36.7, pntLon = 28.783333333333335, pntEle = Just 7.0, pntTime = Nothing}))
  , (LTBT, ICAOData (Point {pntLat = 38.916666666666664, pntLon = 27.85, pntEle = Just 93.0, pntTime = Nothing}))
  , (LTBV, ICAOData (Point {pntLat = 37.03333333333333, pntLon = 27.416666666666668, pntEle = Just 27.0, pntTime = Nothing}))
  , (LTCA, ICAOData (Point {pntLat = 38.6, pntLon = 39.28333333333333, pntEle = Just 902.0, pntTime = Nothing}))
  , (LTCC, ICAOData (Point {pntLat = 37.88333333333333, pntLon = 40.18333333333333, pntEle = Just 686.0, pntTime = Nothing}))
  , (LTCD, ICAOData (Point {pntLat = 39.733333333333334, pntLon = 39.5, pntEle = Just 1156.0, pntTime = Nothing}))
  , (LTCE, ICAOData (Point {pntLat = 39.916666666666664, pntLon = 41.266666666666666, pntEle = Just 1756.0, pntTime = Nothing}))
  , (LTCF, ICAOData (Point {pntLat = 40.6, pntLon = 43.083333333333336, pntEle = Just 1775.0, pntTime = Nothing}))
  , (LTCG, ICAOData (Point {pntLat = 41.0, pntLon = 39.71666666666667, pntEle = Just 35.0, pntTime = Nothing}))
  , (LTCH, ICAOData (Point {pntLat = 37.13333333333333, pntLon = 38.766666666666666, pntEle = Just 547.0, pntTime = Nothing}))
  , (LTCI, ICAOData (Point {pntLat = 38.45, pntLon = 43.31666666666667, pntEle = Just 1667.0, pntTime = Nothing}))
  , (LTCJ, ICAOData (Point {pntLat = 37.86666666666667, pntLon = 41.166666666666664, pntEle = Just 540.0, pntTime = Nothing}))
  , (LWOH, ICAOData (Point {pntLat = 41.11666666666667, pntLon = 20.8, pntEle = Just 760.0, pntTime = Nothing}))
  , (LWSK, ICAOData (Point {pntLat = 41.96666666666667, pntLon = 21.65, pntEle = Just 238.0, pntTime = Nothing}))
  , (LXGB, ICAOData (Point {pntLat = 36.15, pntLon = -5.35, pntEle = Just 5.0, pntTime = Nothing}))
  , (LYBE, ICAOData (Point {pntLat = 44.81666666666667, pntLon = 20.283333333333335, pntEle = Just 96.0, pntTime = Nothing}))
  , (LYNI, ICAOData (Point {pntLat = 43.333333333333336, pntLon = 21.9, pntEle = Just 202.0, pntTime = Nothing}))
  , (LYPR, ICAOData (Point {pntLat = 42.56666666666667, pntLon = 21.033333333333335, pntEle = Just 545.0, pntTime = Nothing}))
  , (LYPZ, ICAOData (Point {pntLat = 45.483333333333334, pntLon = 13.616666666666667, pntEle = Just 2.0, pntTime = Nothing}))
  , (LYTI, ICAOData (Point {pntLat = 42.36666666666667, pntLon = 19.25, pntEle = Just 33.0, pntTime = Nothing}))
  , (LYTV, ICAOData (Point {pntLat = 42.4, pntLon = 18.733333333333334, pntEle = Just 5.0, pntTime = Nothing}))
  , (LYVR, ICAOData (Point {pntLat = 45.15, pntLon = 21.316666666666666, pntEle = Just 83.0, pntTime = Nothing}))
  , (LZIB, ICAOData (Point {pntLat = 48.2, pntLon = 17.2, pntEle = Just 133.0, pntTime = Nothing}))
  , (LZKC, ICAOData (Point {pntLat = 48.93333333333333, pntLon = 22.0, pntEle = Just 177.0, pntTime = Nothing}))
  , (LZLU, ICAOData (Point {pntLat = 48.333333333333336, pntLon = 19.733333333333334, pntEle = Just 214.0, pntTime = Nothing}))
  , (LZTT, ICAOData (Point {pntLat = 49.06666666666667, pntLon = 20.25, pntEle = Just 694.0, pntTime = Nothing}))
  , (MDBH, ICAOData (Point {pntLat = 18.2, pntLon = -71.1, pntEle = Just 3.0, pntTime = Nothing}))
  , (MDHE, ICAOData (Point {pntLat = 18.466666666666665, pntLon = -69.96666666666667, pntEle = Just 58.0, pntTime = Nothing}))
  , (MDPC, ICAOData (Point {pntLat = 18.566666666666666, pntLon = -68.36666666666666, pntEle = Just 12.0, pntTime = Nothing}))
  , (MDPP, ICAOData (Point {pntLat = 19.75, pntLon = -70.55, pntEle = Just 15.0, pntTime = Nothing}))
  , (MDSD, ICAOData (Point {pntLat = 18.433333333333334, pntLon = -69.66666666666667, pntEle = Just 18.0, pntTime = Nothing}))
  , (MDST, ICAOData (Point {pntLat = 19.45, pntLon = -70.7, pntEle = Just 183.0, pntTime = Nothing}))
  , (MGCB, ICAOData (Point {pntLat = 15.466666666666667, pntLon = -90.31666666666666, pntEle = Just 1316.0, pntTime = Nothing}))
  , (MGFL, ICAOData (Point {pntLat = 16.916666666666668, pntLon = -89.88333333333334, pntEle = Just 115.0, pntTime = Nothing}))
  , (MGGT, ICAOData (Point {pntLat = 14.583333333333334, pntLon = -90.51666666666667, pntEle = Just 1489.0, pntTime = Nothing}))
  , (MGHT, ICAOData (Point {pntLat = 15.316666666666666, pntLon = -91.46666666666667, pntEle = Just 1901.0, pntTime = Nothing}))
  , (MGPB, ICAOData (Point {pntLat = 15.716666666666667, pntLon = -88.6, pntEle = Just 1.0, pntTime = Nothing}))
  , (MGRT, ICAOData (Point {pntLat = 14.533333333333333, pntLon = -91.66666666666667, pntEle = Just 239.0, pntTime = Nothing}))
  , (MGSJ, ICAOData (Point {pntLat = 13.916666666666666, pntLon = -90.81666666666666, pntEle = Just 2.0, pntTime = Nothing}))
  , (MHAM, ICAOData (Point {pntLat = 13.266666666666667, pntLon = -87.65, pntEle = Just 5.0, pntTime = Nothing}))
  , (MHCA, ICAOData (Point {pntLat = 14.9, pntLon = -85.93333333333334, pntEle = Just 442.0, pntTime = Nothing}))
  , (MHCH, ICAOData (Point {pntLat = 13.3, pntLon = -87.18333333333334, pntEle = Just 48.0, pntTime = Nothing}))
  , (MHIC, ICAOData (Point {pntLat = 17.4, pntLon = -83.93333333333334, pntEle = Just 9.0, pntTime = Nothing}))
  , (MHLC, ICAOData (Point {pntLat = 15.733333333333333, pntLon = -86.86666666666666, pntEle = Just 3.0, pntTime = Nothing}))
  , (MHLE, ICAOData (Point {pntLat = 14.316666666666666, pntLon = -88.15, pntEle = Just 1100.0, pntTime = Nothing}))
  , (MHLM, ICAOData (Point {pntLat = 15.45, pntLon = -87.93333333333334, pntEle = Just 31.0, pntTime = Nothing}))
  , (MHNO, ICAOData (Point {pntLat = 16.466666666666665, pntLon = -86.06666666666666, pntEle = Just 2.0, pntTime = Nothing}))
  , (MHPL, ICAOData (Point {pntLat = 15.216666666666667, pntLon = -83.8, pntEle = Just 13.0, pntTime = Nothing}))
  , (MHRO, ICAOData (Point {pntLat = 16.316666666666666, pntLon = -86.51666666666667, pntEle = Just 5.0, pntTime = Nothing}))
  , (MHSC, ICAOData (Point {pntLat = 14.383333333333333, pntLon = -87.61666666666666, pntEle = Just 628.0, pntTime = Nothing}))
  , (MHSR, ICAOData (Point {pntLat = 14.783333333333333, pntLon = -88.78333333333333, pntEle = Just 1079.0, pntTime = Nothing}))
  , (MHTE, ICAOData (Point {pntLat = 15.716666666666667, pntLon = -87.48333333333333, pntEle = Just 3.0, pntTime = Nothing}))
  , (MHTG, ICAOData (Point {pntLat = 14.05, pntLon = -87.21666666666667, pntEle = Just 994.0, pntTime = Nothing}))
  , (MHYR, ICAOData (Point {pntLat = 15.166666666666666, pntLon = -87.11666666666666, pntEle = Just 670.0, pntTime = Nothing}))
  , (MKJP, ICAOData (Point {pntLat = 17.933333333333334, pntLon = -76.78333333333333, pntEle = Just 3.0, pntTime = Nothing}))
  , (MKJS, ICAOData (Point {pntLat = 18.5, pntLon = -77.91666666666667, pntEle = Just 1.0, pntTime = Nothing}))
  , (MMAN, ICAOData (Point {pntLat = 25.866666666666667, pntLon = -100.23333333333333, pntEle = Just 448.0, pntTime = Nothing}))
  , (MMAS, ICAOData (Point {pntLat = 21.883333333333333, pntLon = -102.3, pntEle = Just 1874.0, pntTime = Nothing}))
  , (MMCB, ICAOData (Point {pntLat = 18.883333333333333, pntLon = -99.23333333333333, pntEle = Just 1618.0, pntTime = Nothing}))
  , (MMCL, ICAOData (Point {pntLat = 24.816666666666666, pntLon = -107.4, pntEle = Just 39.0, pntTime = Nothing}))
  , (MMCM, ICAOData (Point {pntLat = 18.483333333333334, pntLon = -88.3, pntEle = Just 9.0, pntTime = Nothing}))
  , (MMCN, ICAOData (Point {pntLat = 27.383333333333333, pntLon = -109.81666666666666, pntEle = Just 62.0, pntTime = Nothing}))
  , (MMCP, ICAOData (Point {pntLat = 19.85, pntLon = -90.55, pntEle = Just 5.0, pntTime = Nothing}))
  , (MMEP, ICAOData (Point {pntLat = 21.516666666666666, pntLon = -104.9, pntEle = Just 922.0, pntTime = Nothing}))
  , (MMHO, ICAOData (Point {pntLat = 29.066666666666666, pntLon = -110.95, pntEle = Just 211.0, pntTime = Nothing}))
  , (MMIA, ICAOData (Point {pntLat = 19.266666666666666, pntLon = -103.58333333333333, pntEle = Just 723.0, pntTime = Nothing}))
  , (MMIO, ICAOData (Point {pntLat = 25.45, pntLon = -100.98333333333333, pntEle = Just 1790.0, pntTime = Nothing}))
  , (MMLT, ICAOData (Point {pntLat = 26.016666666666666, pntLon = -111.35, pntEle = Just 15.0, pntTime = Nothing}))
  , (MMMD, ICAOData (Point {pntLat = 20.933333333333334, pntLon = -89.65, pntEle = Just 10.0, pntTime = Nothing}))
  , (MMMV, ICAOData (Point {pntLat = 26.883333333333333, pntLon = -101.41666666666667, pntEle = Just 615.0, pntTime = Nothing}))
  , (MMPB, ICAOData (Point {pntLat = 19.05, pntLon = -98.16666666666667, pntEle = Just 2179.0, pntTime = Nothing}))
  , (MMPG, ICAOData (Point {pntLat = 28.7, pntLon = -100.51666666666667, pntEle = Just 250.0, pntTime = Nothing}))
  , (MMQT, ICAOData (Point {pntLat = 20.6, pntLon = -100.38333333333334, pntEle = Just 1813.0, pntTime = Nothing}))
  , (MMSP, ICAOData (Point {pntLat = 22.15, pntLon = -100.98333333333333, pntEle = Just 1870.0, pntTime = Nothing}))
  , (MMTC, ICAOData (Point {pntLat = 25.533333333333335, pntLon = -103.45, pntEle = Just 1124.0, pntTime = Nothing}))
  , (MMTG, ICAOData (Point {pntLat = 16.75, pntLon = -93.11666666666666, pntEle = Just 528.0, pntTime = Nothing}))
  , (MMTL, ICAOData (Point {pntLat = 20.083333333333332, pntLon = -98.36666666666666, pntEle = Just 2181.0, pntTime = Nothing}))
  , (MNBL, ICAOData (Point {pntLat = 12.0, pntLon = -86.76666666666667, pntEle = Just 5.0, pntTime = Nothing}))
  , (MNCH, ICAOData (Point {pntLat = 12.633333333333333, pntLon = -87.13333333333334, pntEle = Just 53.0, pntTime = Nothing}))
  , (MNJG, ICAOData (Point {pntLat = 13.083333333333334, pntLon = -85.98333333333333, pntEle = Just 985.0, pntTime = Nothing}))
  , (MNJU, ICAOData (Point {pntLat = 12.1, pntLon = -85.36666666666666, pntEle = Just 90.0, pntTime = Nothing}))
  , (MNMG, ICAOData (Point {pntLat = 12.15, pntLon = -86.16666666666667, pntEle = Just 50.0, pntTime = Nothing}))
  , (MNPC, ICAOData (Point {pntLat = 14.05, pntLon = -83.36666666666666, pntEle = Just 20.0, pntTime = Nothing}))
  , (MNRS, ICAOData (Point {pntLat = 11.416666666666666, pntLon = -85.83333333333333, pntEle = Just 53.0, pntTime = Nothing}))
  , (MPDA, ICAOData (Point {pntLat = 8.4, pntLon = -82.41666666666667, pntEle = Just 29.0, pntTime = Nothing}))
  , (MPFS, ICAOData (Point {pntLat = 9.333333333333334, pntLon = -79.98333333333333, pntEle = Just 52.0, pntTime = Nothing}))
  , (MPHO, ICAOData (Point {pntLat = 8.916666666666666, pntLon = -79.6, pntEle = Just 16.0, pntTime = Nothing}))
  , (MPSA, ICAOData (Point {pntLat = 8.083333333333334, pntLon = -80.95, pntEle = Just 83.0, pntTime = Nothing}))
  , (MPTO, ICAOData (Point {pntLat = 9.05, pntLon = -79.36666666666666, pntEle = Just 45.0, pntTime = Nothing}))
  , (MRLB, ICAOData (Point {pntLat = 10.616666666666667, pntLon = -85.43333333333334, pntEle = Just 80.0, pntTime = Nothing}))
  , (MRLM, ICAOData (Point {pntLat = 10.0, pntLon = -83.05, pntEle = Just 5.0, pntTime = Nothing}))
  , (MRNC, ICAOData (Point {pntLat = 10.15, pntLon = -85.45, pntEle = Just 120.0, pntTime = Nothing}))
  , (MROC, ICAOData (Point {pntLat = 10.0, pntLon = -84.21666666666667, pntEle = Just 920.0, pntTime = Nothing}))
  , (MRPM, ICAOData (Point {pntLat = 8.95, pntLon = -83.46666666666667, pntEle = Just 15.0, pntTime = Nothing}))
  , (MSAC, ICAOData (Point {pntLat = 13.566666666666666, pntLon = -89.83333333333333, pntEle = Just 15.0, pntTime = Nothing}))
  , (MSLP, ICAOData (Point {pntLat = 13.433333333333334, pntLon = -89.05, pntEle = Just 25.0, pntTime = Nothing}))
  , (MSSA, ICAOData (Point {pntLat = 13.983333333333333, pntLon = -89.56666666666666, pntEle = Just 725.0, pntTime = Nothing}))
  , (MSSM, ICAOData (Point {pntLat = 13.45, pntLon = -88.11666666666666, pntEle = Just 80.0, pntTime = Nothing}))
  , (MSSS, ICAOData (Point {pntLat = 13.7, pntLon = -89.11666666666666, pntEle = Just 616.0, pntTime = Nothing}))
  , (MTCH, ICAOData (Point {pntLat = 19.75, pntLon = -72.18333333333334, pntEle = Just 2.0, pntTime = Nothing}))
  , (MUBA, ICAOData (Point {pntLat = 20.35, pntLon = -74.5, pntEle = Just 9.0, pntTime = Nothing}))
  , (MUCA, ICAOData (Point {pntLat = 21.783333333333335, pntLon = -78.78333333333333, pntEle = Just 26.0, pntTime = Nothing}))
  , (MUCF, ICAOData (Point {pntLat = 22.15, pntLon = -80.4, pntEle = Just 39.0, pntTime = Nothing}))
  , (MUCM, ICAOData (Point {pntLat = 21.416666666666668, pntLon = -77.85, pntEle = Just 122.0, pntTime = Nothing}))
  , (MUCU, ICAOData (Point {pntLat = 19.966666666666665, pntLon = -75.85, pntEle = Just 69.0, pntTime = Nothing}))
  , (MUGM, ICAOData (Point {pntLat = 19.916666666666668, pntLon = -75.2, pntEle = Just 17.0, pntTime = Nothing}))
  , (MUGT, ICAOData (Point {pntLat = 20.083333333333332, pntLon = -75.15, pntEle = Just 8.0, pntTime = Nothing}))
  , (MUHA, ICAOData (Point {pntLat = 22.983333333333334, pntLon = -82.4, pntEle = Just 59.0, pntTime = Nothing}))
  , (MUMZ, ICAOData (Point {pntLat = 20.333333333333332, pntLon = -77.11666666666666, pntEle = Just 60.0, pntTime = Nothing}))
  , (MUNG, ICAOData (Point {pntLat = 21.833333333333332, pntLon = -82.78333333333333, pntEle = Just 23.0, pntTime = Nothing}))
  , (MUPR, ICAOData (Point {pntLat = 22.416666666666668, pntLon = -83.68333333333334, pntEle = Just 37.0, pntTime = Nothing}))
  , (MUVR, ICAOData (Point {pntLat = 23.133333333333333, pntLon = -81.28333333333333, pntEle = Just 3.0, pntTime = Nothing}))
  , (MUVT, ICAOData (Point {pntLat = 20.95, pntLon = -76.95, pntEle = Just 106.0, pntTime = Nothing}))
  , (MWCR, ICAOData (Point {pntLat = 19.283333333333335, pntLon = -81.35, pntEle = Just 3.0, pntTime = Nothing}))
  , (MYBS, ICAOData (Point {pntLat = 25.733333333333334, pntLon = -79.3, pntEle = Just 2.0, pntTime = Nothing}))
  , (MYEG, ICAOData (Point {pntLat = 23.466666666666665, pntLon = -75.76666666666667, pntEle = Just 2.0, pntTime = Nothing}))
  , (MYGF, ICAOData (Point {pntLat = 26.55, pntLon = -78.7, pntEle = Just 2.0, pntTime = Nothing}))
  , (MYGW, ICAOData (Point {pntLat = 26.7, pntLon = -78.96666666666667, pntEle = Just 2.0, pntTime = Nothing}))
  , (MYIG, ICAOData (Point {pntLat = 20.95, pntLon = -73.68333333333334, pntEle = Just 2.0, pntTime = Nothing}))
  , (MYNN, ICAOData (Point {pntLat = 25.05, pntLon = -77.46666666666667, pntEle = Just 3.0, pntTime = Nothing}))
  , (MYSM, ICAOData (Point {pntLat = 24.05, pntLon = -74.53333333333333, pntEle = Just 3.0, pntTime = Nothing}))
  , (MZBZ, ICAOData (Point {pntLat = 17.533333333333335, pntLon = -88.3, pntEle = Just 5.0, pntTime = Nothing}))
  , (NCRG, ICAOData (Point {pntLat = -21.2, pntLon = -159.81666666666666, pntEle = Just 7.0, pntTime = Nothing}))
  , (NFFN, ICAOData (Point {pntLat = -17.75, pntLon = 177.45, pntEle = Just 13.0, pntTime = Nothing}))
  , (NFNA, ICAOData (Point {pntLat = -18.05, pntLon = 178.56666666666666, pntEle = Just 5.0, pntTime = Nothing}))
  , (NFNK, ICAOData (Point {pntLat = -18.233333333333334, pntLon = -178.8, pntEle = Just 2.0, pntTime = Nothing}))
  , (NFNR, ICAOData (Point {pntLat = -12.5, pntLon = 177.05, pntEle = Just 26.0, pntTime = Nothing}))
  , (NFTF, ICAOData (Point {pntLat = -21.233333333333334, pntLon = -175.15, pntEle = Just 38.0, pntTime = Nothing}))
  , (NFTL, ICAOData (Point {pntLat = -19.8, pntLon = -174.35, pntEle = Just 2.0, pntTime = Nothing}))
  , (NGBR, ICAOData (Point {pntLat = -1.35, pntLon = 176.0, pntEle = Just 2.0, pntTime = Nothing}))
  , (NGFO, ICAOData (Point {pntLat = -5.666666666666667, pntLon = 176.13333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (NGFU, ICAOData (Point {pntLat = -8.516666666666667, pntLon = 179.21666666666667, pntEle = Just 1.0, pntTime = Nothing}))
  , (NGTA, ICAOData (Point {pntLat = 1.35, pntLon = 172.91666666666666, pntEle = Just 2.0, pntTime = Nothing}))
  , (NGTR, ICAOData (Point {pntLat = -2.6666666666666665, pntLon = 176.83333333333334, pntEle = Just 4.0, pntTime = Nothing}))
  , (NGTU, ICAOData (Point {pntLat = 3.033333333333333, pntLon = 172.78333333333333, pntEle = Just 1.0, pntTime = Nothing}))
  , (NIUE, ICAOData (Point {pntLat = -19.066666666666666, pntLon = -169.91666666666666, pntEle = Just 20.0, pntTime = Nothing}))
  , (NLWF, ICAOData (Point {pntLat = -14.316666666666666, pntLon = -178.11666666666667, pntEle = Just 6.0, pntTime = Nothing}))
  , (NLWW, ICAOData (Point {pntLat = -13.233333333333333, pntLon = -176.16666666666666, pntEle = Just 23.0, pntTime = Nothing}))
  , (NSAP, ICAOData (Point {pntLat = -13.8, pntLon = -171.78333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (NSFA, ICAOData (Point {pntLat = -13.816666666666666, pntLon = -172.0, pntEle = Just 5.0, pntTime = Nothing}))
  , (NSTU, ICAOData (Point {pntLat = -14.333333333333334, pntLon = -170.71666666666667, pntEle = Just 3.0, pntTime = Nothing}))
  , (NTAA, ICAOData (Point {pntLat = -17.55, pntLon = -149.61666666666667, pntEle = Just 2.0, pntTime = Nothing}))
  , (NTAT, ICAOData (Point {pntLat = -23.35, pntLon = -149.48333333333332, pntEle = Just 2.0, pntTime = Nothing}))
  , (NTTB, ICAOData (Point {pntLat = -16.45, pntLon = -151.75, pntEle = Just 4.0, pntTime = Nothing}))
  , (NTTG, ICAOData (Point {pntLat = -14.966666666666667, pntLon = -147.66666666666666, pntEle = Just 3.0, pntTime = Nothing}))
  , (NTTO, ICAOData (Point {pntLat = -18.066666666666666, pntLon = -140.95, pntEle = Just 2.0, pntTime = Nothing}))
  , (NTTX, ICAOData (Point {pntLat = -21.816666666666666, pntLon = -138.8, pntEle = Just 2.0, pntTime = Nothing}))
  , (NVSC, ICAOData (Point {pntLat = -13.85, pntLon = 167.55, pntEle = Just 17.0, pntTime = Nothing}))
  , (NVSL, ICAOData (Point {pntLat = -16.416666666666668, pntLon = 167.8, pntEle = Just 26.0, pntTime = Nothing}))
  , (NVSS, ICAOData (Point {pntLat = -15.516666666666667, pntLon = 167.21666666666667, pntEle = Just 41.0, pntTime = Nothing}))
  , (NVVV, ICAOData (Point {pntLat = -17.7, pntLon = 168.3, pntEle = Just 20.0, pntTime = Nothing}))
  , (NWWE, ICAOData (Point {pntLat = -22.6, pntLon = 167.45, pntEle = Just 96.0, pntTime = Nothing}))
  , (NWWK, ICAOData (Point {pntLat = -20.566666666666666, pntLon = 164.28333333333333, pntEle = Just 23.0, pntTime = Nothing}))
  , (NWWL, ICAOData (Point {pntLat = -20.766666666666666, pntLon = 167.23333333333332, pntEle = Just 28.0, pntTime = Nothing}))
  , (NWWN, ICAOData (Point {pntLat = -22.266666666666666, pntLon = 166.45, pntEle = Just 69.0, pntTime = Nothing}))
  , (NWWR, ICAOData (Point {pntLat = -21.483333333333334, pntLon = 168.03333333333333, pntEle = Just 43.0, pntTime = Nothing}))
  , (NWWV, ICAOData (Point {pntLat = -20.65, pntLon = 166.58333333333334, pntEle = Just 7.0, pntTime = Nothing}))
  , (NWWW, ICAOData (Point {pntLat = -22.016666666666666, pntLon = 166.21666666666667, pntEle = Just 16.0, pntTime = Nothing}))
  , (NZAA, ICAOData (Point {pntLat = -37.016666666666666, pntLon = 174.8, pntEle = Just 7.0, pntTime = Nothing}))
  , (NZCH, ICAOData (Point {pntLat = -43.483333333333334, pntLon = 172.55, pntEle = Just 38.0, pntTime = Nothing}))
  , (NZCI, ICAOData (Point {pntLat = -43.95, pntLon = -176.56666666666666, pntEle = Just 44.0, pntTime = Nothing}))
  , (NZCM, ICAOData (Point {pntLat = -77.86666666666666, pntLon = 166.96666666666667, pntEle = Just 8.0, pntTime = Nothing}))
  , (NZDN, ICAOData (Point {pntLat = -45.93333333333333, pntLon = 170.2, pntEle = Just 1.0, pntTime = Nothing}))
  , (NZGS, ICAOData (Point {pntLat = -38.666666666666664, pntLon = 177.98333333333332, pntEle = Just 5.0, pntTime = Nothing}))
  , (NZHK, ICAOData (Point {pntLat = -42.71666666666667, pntLon = 170.98333333333332, pntEle = Just 45.0, pntTime = Nothing}))
  , (NZKI, ICAOData (Point {pntLat = -42.416666666666664, pntLon = 173.7, pntEle = Just 101.0, pntTime = Nothing}))
  , (NZNP, ICAOData (Point {pntLat = -39.016666666666666, pntLon = 174.18333333333334, pntEle = Just 28.0, pntTime = Nothing}))
  , (NZNV, ICAOData (Point {pntLat = -46.7, pntLon = 168.55, pntEle = Just 0.0, pntTime = Nothing}))
  , (NZOH, ICAOData (Point {pntLat = -40.2, pntLon = 175.36666666666667, pntEle = Just 50.0, pntTime = Nothing}))
  , (NZPP, ICAOData (Point {pntLat = -40.9, pntLon = 174.98333333333332, pntEle = Just 7.0, pntTime = Nothing}))
  , (NZRN, ICAOData (Point {pntLat = -29.25, pntLon = -177.91666666666666, pntEle = Just 38.0, pntTime = Nothing}))
  , (NZRO, ICAOData (Point {pntLat = -38.11666666666667, pntLon = 176.31666666666666, pntEle = Just 285.0, pntTime = Nothing}))
  , (NZSP, ICAOData (Point {pntLat = -90.0, pntLon = 0.0, pntEle = Just 2830.0, pntTime = Nothing}))
  , (NZTG, ICAOData (Point {pntLat = -37.666666666666664, pntLon = 176.2, pntEle = Just 4.0, pntTime = Nothing}))
  , (NZWN, ICAOData (Point {pntLat = -41.333333333333336, pntLon = 174.8, pntEle = Just 12.0, pntTime = Nothing}))
  , (NZWP, ICAOData (Point {pntLat = -36.78333333333333, pntLon = 174.63333333333333, pntEle = Just 30.0, pntTime = Nothing}))
  , (OAFR, ICAOData (Point {pntLat = 32.36666666666667, pntLon = 62.18333333333333, pntEle = Just 700.0, pntTime = Nothing}))
  , (OAFZ, ICAOData (Point {pntLat = 37.11666666666667, pntLon = 70.51666666666667, pntEle = Just 1200.0, pntTime = Nothing}))
  , (OAHR, ICAOData (Point {pntLat = 34.21666666666667, pntLon = 62.21666666666667, pntEle = Just 964.0, pntTime = Nothing}))
  , (OAJL, ICAOData (Point {pntLat = 34.43333333333333, pntLon = 70.46666666666667, pntEle = Just 580.0, pntTime = Nothing}))
  , (OAJS, ICAOData (Point {pntLat = 35.13333333333333, pntLon = 69.25, pntEle = Just 1630.0, pntTime = Nothing}))
  , (OAKB, ICAOData (Point {pntLat = 34.63333333333333, pntLon = 69.2, pntEle = Just 1789.0, pntTime = Nothing}))
  , (OAMS, ICAOData (Point {pntLat = 36.7, pntLon = 67.2, pntEle = Just 378.0, pntTime = Nothing}))
  , (OASD, ICAOData (Point {pntLat = 33.4, pntLon = 62.28333333333333, pntEle = Just 1150.0, pntTime = Nothing}))
  , (OASG, ICAOData (Point {pntLat = 36.666666666666664, pntLon = 65.71666666666667, pntEle = Just 360.0, pntTime = Nothing}))
  , (OAZB, ICAOData (Point {pntLat = 36.5, pntLon = 71.25, pntEle = Just 2600.0, pntTime = Nothing}))
  , (OAZG, ICAOData (Point {pntLat = 31.0, pntLon = 61.85, pntEle = Just 478.0, pntTime = Nothing}))
  , (OBBI, ICAOData (Point {pntLat = 26.266666666666666, pntLon = 50.65, pntEle = Just 2.0, pntTime = Nothing}))
  , (OEAB, ICAOData (Point {pntLat = 18.233333333333334, pntLon = 42.65, pntEle = Just 2090.0, pntTime = Nothing}))
  , (OEAH, ICAOData (Point {pntLat = 25.3, pntLon = 49.483333333333334, pntEle = Just 179.0, pntTime = Nothing}))
  , (OEBA, ICAOData (Point {pntLat = 20.3, pntLon = 41.63333333333333, pntEle = Just 1652.0, pntTime = Nothing}))
  , (OEBH, ICAOData (Point {pntLat = 19.983333333333334, pntLon = 42.61666666666667, pntEle = Just 1167.0, pntTime = Nothing}))
  , (OEDF, ICAOData (Point {pntLat = 26.466666666666665, pntLon = 49.78333333333333, pntEle = Just 22.0, pntTime = Nothing}))
  , (OEDR, ICAOData (Point {pntLat = 26.266666666666666, pntLon = 50.15, pntEle = Just 26.0, pntTime = Nothing}))
  , (OEDW, ICAOData (Point {pntLat = 24.5, pntLon = 44.35, pntEle = Just 990.0, pntTime = Nothing}))
  , (OEGN, ICAOData (Point {pntLat = 16.9, pntLon = 42.583333333333336, pntEle = Just 6.0, pntTime = Nothing}))
  , (OEGS, ICAOData (Point {pntLat = 26.3, pntLon = 43.766666666666666, pntEle = Just 648.0, pntTime = Nothing}))
  , (OEGT, ICAOData (Point {pntLat = 31.4, pntLon = 37.266666666666666, pntEle = Just 509.0, pntTime = Nothing}))
  , (OEHL, ICAOData (Point {pntLat = 27.433333333333334, pntLon = 41.68333333333333, pntEle = Just 1015.0, pntTime = Nothing}))
  , (OEJD, ICAOData (Point {pntLat = 21.5, pntLon = 39.2, pntEle = Just 15.0, pntTime = Nothing}))
  , (OEJN, ICAOData (Point {pntLat = 21.7, pntLon = 39.18333333333333, pntEle = Just 15.0, pntTime = Nothing}))
  , (OEKK, ICAOData (Point {pntLat = 27.9, pntLon = 45.53333333333333, pntEle = Just 413.0, pntTime = Nothing}))
  , (OEKM, ICAOData (Point {pntLat = 18.3, pntLon = 42.8, pntEle = Just 2066.0, pntTime = Nothing}))
  , (OEMA, ICAOData (Point {pntLat = 24.55, pntLon = 39.7, pntEle = Just 654.0, pntTime = Nothing}))
  , (OEMK, ICAOData (Point {pntLat = 21.433333333333334, pntLon = 39.766666666666666, pntEle = Just 310.0, pntTime = Nothing}))
  , (OENG, ICAOData (Point {pntLat = 17.616666666666667, pntLon = 44.416666666666664, pntEle = Just 1212.0, pntTime = Nothing}))
  , (OEPA, ICAOData (Point {pntLat = 28.316666666666666, pntLon = 46.11666666666667, pntEle = Just 358.0, pntTime = Nothing}))
  , (OERF, ICAOData (Point {pntLat = 29.616666666666667, pntLon = 43.483333333333334, pntEle = Just 449.0, pntTime = Nothing}))
  , (OERK, ICAOData (Point {pntLat = 24.933333333333334, pntLon = 46.71666666666667, pntEle = Just 614.0, pntTime = Nothing}))
  , (OERR, ICAOData (Point {pntLat = 30.9, pntLon = 41.13333333333333, pntEle = Just 555.0, pntTime = Nothing}))
  , (OERY, ICAOData (Point {pntLat = 24.716666666666665, pntLon = 46.733333333333334, pntEle = Just 635.0, pntTime = Nothing}))
  , (OESH, ICAOData (Point {pntLat = 17.466666666666665, pntLon = 47.11666666666667, pntEle = Just 720.0, pntTime = Nothing}))
  , (OESK, ICAOData (Point {pntLat = 29.783333333333335, pntLon = 40.1, pntEle = Just 689.0, pntTime = Nothing}))
  , (OETB, ICAOData (Point {pntLat = 28.366666666666667, pntLon = 36.6, pntEle = Just 778.0, pntTime = Nothing}))
  , (OETF, ICAOData (Point {pntLat = 21.483333333333334, pntLon = 40.55, pntEle = Just 1478.0, pntTime = Nothing}))
  , (OETR, ICAOData (Point {pntLat = 31.683333333333334, pntLon = 38.733333333333334, pntEle = Just 813.0, pntTime = Nothing}))
  , (OEWD, ICAOData (Point {pntLat = 20.5, pntLon = 45.2, pntEle = Just 624.0, pntTime = Nothing}))
  , (OEWJ, ICAOData (Point {pntLat = 26.2, pntLon = 36.46666666666667, pntEle = Just 20.0, pntTime = Nothing}))
  , (OEYN, ICAOData (Point {pntLat = 24.15, pntLon = 38.06666666666667, pntEle = Just 8.0, pntTime = Nothing}))
  , (OIAA, ICAOData (Point {pntLat = 30.366666666666667, pntLon = 48.25, pntEle = Just 3.0, pntTime = Nothing}))
  , (OIAG, ICAOData (Point {pntLat = 30.766666666666666, pntLon = 49.666666666666664, pntEle = Just 29.0, pntTime = Nothing}))
  , (OIAH, ICAOData (Point {pntLat = 30.433333333333334, pntLon = 50.766666666666666, pntEle = Just 738.0, pntTime = Nothing}))
  , (OIAI, ICAOData (Point {pntLat = 31.983333333333334, pntLon = 49.266666666666666, pntEle = Just 372.0, pntTime = Nothing}))
  , (OIAW, ICAOData (Point {pntLat = 31.333333333333332, pntLon = 48.666666666666664, pntEle = Just 20.0, pntTime = Nothing}))
  , (OIBB, ICAOData (Point {pntLat = 28.983333333333334, pntLon = 50.833333333333336, pntEle = Just 19.0, pntTime = Nothing}))
  , (OIBL, ICAOData (Point {pntLat = 26.583333333333332, pntLon = 54.833333333333336, pntEle = Just 14.0, pntTime = Nothing}))
  , (OICC, ICAOData (Point {pntLat = 34.266666666666666, pntLon = 47.11666666666667, pntEle = Just 1320.0, pntTime = Nothing}))
  , (OICG, ICAOData (Point {pntLat = 34.516666666666666, pntLon = 45.583333333333336, pntEle = Just 378.0, pntTime = Nothing}))
  , (OICS, ICAOData (Point {pntLat = 35.333333333333336, pntLon = 47.0, pntEle = Just 1373.0, pntTime = Nothing}))
  , (OIFK, ICAOData (Point {pntLat = 33.983333333333334, pntLon = 51.45, pntEle = Just 982.0, pntTime = Nothing}))
  , (OIFM, ICAOData (Point {pntLat = 32.46666666666667, pntLon = 51.71666666666667, pntEle = Just 1590.0, pntTime = Nothing}))
  , (OIFS, ICAOData (Point {pntLat = 32.333333333333336, pntLon = 50.85, pntEle = Just 2078.0, pntTime = Nothing}))
  , (OIGG, ICAOData (Point {pntLat = 37.2, pntLon = 49.63333333333333, pntEle = Just (-7.0), pntTime = Nothing}))
  , (OIHR, ICAOData (Point {pntLat = 34.1, pntLon = 49.4, pntEle = Just 1720.0, pntTime = Nothing}))
  , (OIII, ICAOData (Point {pntLat = 35.68333333333333, pntLon = 51.35, pntEle = Just 1204.0, pntTime = Nothing}))
  , (OIIK, ICAOData (Point {pntLat = 36.25, pntLon = 50.0, pntEle = Just 1278.0, pntTime = Nothing}))
  , (OIIS, ICAOData (Point {pntLat = 35.55, pntLon = 53.38333333333333, pntEle = Just 1171.0, pntTime = Nothing}))
  , (OIKB, ICAOData (Point {pntLat = 27.216666666666665, pntLon = 56.36666666666667, pntEle = Just 10.0, pntTime = Nothing}))
  , (OIKK, ICAOData (Point {pntLat = 30.25, pntLon = 56.96666666666667, pntEle = Just 1748.0, pntTime = Nothing}))
  , (OIKM, ICAOData (Point {pntLat = 29.1, pntLon = 58.4, pntEle = Just 1067.0, pntTime = Nothing}))
  , (OIMB, ICAOData (Point {pntLat = 32.86666666666667, pntLon = 59.2, pntEle = Just 1491.0, pntTime = Nothing}))
  , (OIMH, ICAOData (Point {pntLat = 35.266666666666666, pntLon = 59.21666666666667, pntEle = Just 1333.0, pntTime = Nothing}))
  , (OIMM, ICAOData (Point {pntLat = 36.266666666666666, pntLon = 59.63333333333333, pntEle = Just 989.0, pntTime = Nothing}))
  , (OIMN, ICAOData (Point {pntLat = 37.46666666666667, pntLon = 57.333333333333336, pntEle = Just 1074.0, pntTime = Nothing}))
  , (OIMS, ICAOData (Point {pntLat = 36.21666666666667, pntLon = 57.666666666666664, pntEle = Just 941.0, pntTime = Nothing}))
  , (OIMT, ICAOData (Point {pntLat = 33.6, pntLon = 56.916666666666664, pntEle = Just 710.0, pntTime = Nothing}))
  , (OING, ICAOData (Point {pntLat = 36.81666666666667, pntLon = 54.46666666666667, pntEle = Just 155.0, pntTime = Nothing}))
  , (OINR, ICAOData (Point {pntLat = 36.9, pntLon = 50.666666666666664, pntEle = Just (-23.0), pntTime = Nothing}))
  , (OISA, ICAOData (Point {pntLat = 31.183333333333334, pntLon = 52.666666666666664, pntEle = Just 2004.0, pntTime = Nothing}))
  , (OISF, ICAOData (Point {pntLat = 28.966666666666665, pntLon = 53.68333333333333, pntEle = Just 1383.0, pntTime = Nothing}))
  , (OISS, ICAOData (Point {pntLat = 29.533333333333335, pntLon = 52.583333333333336, pntEle = Just 1486.0, pntTime = Nothing}))
  , (OITK, ICAOData (Point {pntLat = 38.55, pntLon = 44.96666666666667, pntEle = Just 1107.0, pntTime = Nothing}))
  , (OITR, ICAOData (Point {pntLat = 37.53333333333333, pntLon = 45.083333333333336, pntEle = Just 1297.0, pntTime = Nothing}))
  , (OITS, ICAOData (Point {pntLat = 36.25, pntLon = 46.266666666666666, pntEle = Just 1493.0, pntTime = Nothing}))
  , (OITT, ICAOData (Point {pntLat = 38.083333333333336, pntLon = 46.28333333333333, pntEle = Just 1367.0, pntTime = Nothing}))
  , (OITZ, ICAOData (Point {pntLat = 36.68333333333333, pntLon = 48.483333333333334, pntEle = Just 1663.0, pntTime = Nothing}))
  , (OIYY, ICAOData (Point {pntLat = 31.9, pntLon = 54.4, pntEle = Just 1238.0, pntTime = Nothing}))
  , (OIZB, ICAOData (Point {pntLat = 31.333333333333332, pntLon = 61.483333333333334, pntEle = Just 489.0, pntTime = Nothing}))
  , (OIZC, ICAOData (Point {pntLat = 25.416666666666668, pntLon = 60.75, pntEle = Just 6.0, pntTime = Nothing}))
  , (OIZH, ICAOData (Point {pntLat = 29.466666666666665, pntLon = 60.88333333333333, pntEle = Just 1370.0, pntTime = Nothing}))
  , (OIZI, ICAOData (Point {pntLat = 27.2, pntLon = 60.7, pntEle = Just 591.0, pntTime = Nothing}))
  , (OIZJ, ICAOData (Point {pntLat = 26.633333333333333, pntLon = 57.766666666666666, pntEle = Just 3.0, pntTime = Nothing}))
  , (OJAI, ICAOData (Point {pntLat = 31.666666666666668, pntLon = 35.96666666666667, pntEle = Just 721.0, pntTime = Nothing}))
  , (OJAM, ICAOData (Point {pntLat = 31.983333333333334, pntLon = 35.983333333333334, pntEle = Just 767.0, pntTime = Nothing}))
  , (OJAQ, ICAOData (Point {pntLat = 29.633333333333333, pntLon = 35.016666666666666, pntEle = Just 53.0, pntTime = Nothing}))
  , (OJBD, ICAOData (Point {pntLat = 32.55, pntLon = 35.85, pntEle = Just 618.0, pntTime = Nothing}))
  , (OJHF, ICAOData (Point {pntLat = 32.2, pntLon = 37.13333333333333, pntEle = Just 668.0, pntTime = Nothing}))
  , (OJHR, ICAOData (Point {pntLat = 32.5, pntLon = 38.2, pntEle = Just 686.0, pntTime = Nothing}))
  , (OJMF, ICAOData (Point {pntLat = 32.36666666666667, pntLon = 36.25, pntEle = Just 686.0, pntTime = Nothing}))
  , (OJMN, ICAOData (Point {pntLat = 30.166666666666668, pntLon = 35.78333333333333, pntEle = Just 1069.0, pntTime = Nothing}))
  , (OKBK, ICAOData (Point {pntLat = 29.216666666666665, pntLon = 47.983333333333334, pntEle = Just 55.0, pntTime = Nothing}))
  , (OLBA, ICAOData (Point {pntLat = 33.81666666666667, pntLon = 35.483333333333334, pntEle = Just 29.0, pntTime = Nothing}))
  , (OMAA, ICAOData (Point {pntLat = 24.433333333333334, pntLon = 54.65, pntEle = Just 16.0, pntTime = Nothing}))
  , (OMAD, ICAOData (Point {pntLat = 24.433333333333334, pntLon = 54.46666666666667, pntEle = Just 5.0, pntTime = Nothing}))
  , (OMAL, ICAOData (Point {pntLat = 24.266666666666666, pntLon = 55.6, pntEle = Just 265.0, pntTime = Nothing}))
  , (OMDB, ICAOData (Point {pntLat = 25.25, pntLon = 55.333333333333336, pntEle = Just 8.0, pntTime = Nothing}))
  , (OMFJ, ICAOData (Point {pntLat = 25.1, pntLon = 56.333333333333336, pntEle = Just 28.0, pntTime = Nothing}))
  , (OMRK, ICAOData (Point {pntLat = 25.616666666666667, pntLon = 55.93333333333333, pntEle = Just 31.0, pntTime = Nothing}))
  , (OMSJ, ICAOData (Point {pntLat = 25.333333333333332, pntLon = 55.516666666666666, pntEle = Just 35.0, pntTime = Nothing}))
  , (OOBR, ICAOData (Point {pntLat = 24.233333333333334, pntLon = 55.78333333333333, pntEle = Just 299.0, pntTime = Nothing}))
  , (OOFD, ICAOData (Point {pntLat = 22.35, pntLon = 56.483333333333334, pntEle = Just 170.0, pntTime = Nothing}))
  , (OOKB, ICAOData (Point {pntLat = 26.216666666666665, pntLon = 56.233333333333334, pntEle = Just 3.0, pntTime = Nothing}))
  , (OOMA, ICAOData (Point {pntLat = 20.666666666666668, pntLon = 58.9, pntEle = Just 19.0, pntTime = Nothing}))
  , (OOMS, ICAOData (Point {pntLat = 23.583333333333332, pntLon = 58.28333333333333, pntEle = Just 8.0, pntTime = Nothing}))
  , (OOSA, ICAOData (Point {pntLat = 17.033333333333335, pntLon = 54.083333333333336, pntEle = Just 20.0, pntTime = Nothing}))
  , (OOSH, ICAOData (Point {pntLat = 24.466666666666665, pntLon = 56.63333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (OOSQ, ICAOData (Point {pntLat = 23.066666666666666, pntLon = 57.65, pntEle = Just 1755.0, pntTime = Nothing}))
  , (OOSR, ICAOData (Point {pntLat = 22.533333333333335, pntLon = 59.483333333333334, pntEle = Just 14.0, pntTime = Nothing}))
  , (OOTH, ICAOData (Point {pntLat = 17.666666666666668, pntLon = 54.03333333333333, pntEle = Just 467.0, pntTime = Nothing}))
  , (OPDI, ICAOData (Point {pntLat = 31.816666666666666, pntLon = 70.91666666666667, pntEle = Just 172.0, pntTime = Nothing}))
  , (OPJA, ICAOData (Point {pntLat = 28.3, pntLon = 68.46666666666667, pntEle = Just 55.0, pntTime = Nothing}))
  , (OPJI, ICAOData (Point {pntLat = 25.066666666666666, pntLon = 61.8, pntEle = Just 56.0, pntTime = Nothing}))
  , (OPKC, ICAOData (Point {pntLat = 24.9, pntLon = 67.13333333333334, pntEle = Just 21.0, pntTime = Nothing}))
  , (OPKD, ICAOData (Point {pntLat = 25.383333333333333, pntLon = 68.41666666666667, pntEle = Just 40.0, pntTime = Nothing}))
  , (OPLA, ICAOData (Point {pntLat = 31.516666666666666, pntLon = 74.4, pntEle = Just 216.0, pntTime = Nothing}))
  , (OPLH, ICAOData (Point {pntLat = 31.55, pntLon = 74.33333333333333, pntEle = Just 214.0, pntTime = Nothing}))
  , (OPMI, ICAOData (Point {pntLat = 32.55, pntLon = 71.51666666666667, pntEle = Just 210.0, pntTime = Nothing}))
  , (OPMT, ICAOData (Point {pntLat = 30.2, pntLon = 71.43333333333334, pntEle = Just 122.0, pntTime = Nothing}))
  , (OPNH, ICAOData (Point {pntLat = 26.25, pntLon = 68.36666666666666, pntEle = Just 37.0, pntTime = Nothing}))
  , (OPPG, ICAOData (Point {pntLat = 26.966666666666665, pntLon = 64.1, pntEle = Just 980.0, pntTime = Nothing}))
  , (OPPS, ICAOData (Point {pntLat = 34.016666666666666, pntLon = 71.58333333333333, pntEle = Just 359.0, pntTime = Nothing}))
  , (OPQT, ICAOData (Point {pntLat = 30.25, pntLon = 66.88333333333334, pntEle = Just 1587.0, pntTime = Nothing}))
  , (OPRN, ICAOData (Point {pntLat = 33.61666666666667, pntLon = 73.1, pntEle = Just 507.0, pntTime = Nothing}))
  , (OPRS, ICAOData (Point {pntLat = 34.06666666666667, pntLon = 71.98333333333333, pntEle = Just 315.0, pntTime = Nothing}))
  , (OPSB, ICAOData (Point {pntLat = 29.55, pntLon = 67.88333333333334, pntEle = Just 133.0, pntTime = Nothing}))
  , (OPSR, ICAOData (Point {pntLat = 32.05, pntLon = 72.66666666666667, pntEle = Just 187.0, pntTime = Nothing}))
  , (ORBB, ICAOData (Point {pntLat = 33.233333333333334, pntLon = 44.233333333333334, pntEle = Just 34.0, pntTime = Nothing}))
  , (ORBM, ICAOData (Point {pntLat = 36.31666666666667, pntLon = 43.15, pntEle = Just 223.0, pntTime = Nothing}))
  , (ORMS, ICAOData (Point {pntLat = 30.416666666666668, pntLon = 47.65, pntEle = Just 19.0, pntTime = Nothing}))
  , (OSAP, ICAOData (Point {pntLat = 36.18333333333333, pntLon = 37.2, pntEle = Just 393.0, pntTime = Nothing}))
  , (OSDI, ICAOData (Point {pntLat = 33.416666666666664, pntLon = 36.516666666666666, pntEle = Just 608.0, pntTime = Nothing}))
  , (OSDZ, ICAOData (Point {pntLat = 35.31666666666667, pntLon = 40.15, pntEle = Just 215.0, pntTime = Nothing}))
  , (OSKL, ICAOData (Point {pntLat = 37.05, pntLon = 41.21666666666667, pntEle = Just 455.0, pntTime = Nothing}))
  , (OSLK, ICAOData (Point {pntLat = 35.53333333333333, pntLon = 35.766666666666666, pntEle = Just 7.0, pntTime = Nothing}))
  , (OSPR, ICAOData (Point {pntLat = 34.55, pntLon = 38.3, pntEle = Just 408.0, pntTime = Nothing}))
  , (OTBD, ICAOData (Point {pntLat = 25.25, pntLon = 51.56666666666667, pntEle = Just 11.0, pntTime = Nothing}))
  , (OYAA, ICAOData (Point {pntLat = 12.666666666666666, pntLon = 45.03333333333333, pntEle = Just 3.0, pntTime = Nothing}))
  , (OYAR, ICAOData (Point {pntLat = 14.65, pntLon = 49.38333333333333, pntEle = Just 16.0, pntTime = Nothing}))
  , (OYAT, ICAOData (Point {pntLat = 14.516666666666667, pntLon = 46.85, pntEle = Just 1067.0, pntTime = Nothing}))
  , (OYHD, ICAOData (Point {pntLat = 14.75, pntLon = 42.983333333333334, pntEle = Just 115.0, pntTime = Nothing}))
  , (OYMB, ICAOData (Point {pntLat = 15.433333333333334, pntLon = 45.333333333333336, pntEle = Just 1000.0, pntTime = Nothing}))
  , (OYMC, ICAOData (Point {pntLat = 13.25, pntLon = 44.13333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (OYMK, ICAOData (Point {pntLat = 13.25, pntLon = 43.28333333333333, pntEle = Just 3.0, pntTime = Nothing}))
  , (OYSN, ICAOData (Point {pntLat = 15.516666666666667, pntLon = 44.18333333333333, pntEle = Just 2206.0, pntTime = Nothing}))
  , (OYSQ, ICAOData (Point {pntLat = 12.633333333333333, pntLon = 53.9, pntEle = Just 45.0, pntTime = Nothing}))
  , (OYTZ, ICAOData (Point {pntLat = 13.683333333333334, pntLon = 44.13333333333333, pntEle = Just 1402.0, pntTime = Nothing}))
  , (OYZM, ICAOData (Point {pntLat = 16.2, pntLon = 44.78333333333333, pntEle = Just 1900.0, pntTime = Nothing}))
  , (PAAQ, ICAOData (Point {pntLat = 61.6, pntLon = -149.1, pntEle = Just 76.0, pntTime = Nothing}))
  , (PABA, ICAOData (Point {pntLat = 70.13333333333334, pntLon = -143.56666666666666, pntEle = Just 2.0, pntTime = Nothing}))
  , (PABE, ICAOData (Point {pntLat = 60.78333333333333, pntLon = -161.83333333333334, pntEle = Just 37.0, pntTime = Nothing}))
  , (PABI, ICAOData (Point {pntLat = 64.0, pntLon = -145.71666666666667, pntEle = Just 389.0, pntTime = Nothing}))
  , (PABL, ICAOData (Point {pntLat = 65.96666666666667, pntLon = -161.15, pntEle = Just 7.0, pntTime = Nothing}))
  , (PABR, ICAOData (Point {pntLat = 71.28333333333333, pntLon = -156.76666666666668, pntEle = Just 13.0, pntTime = Nothing}))
  , (PABT, ICAOData (Point {pntLat = 66.9, pntLon = -151.51666666666668, pntEle = Just 196.0, pntTime = Nothing}))
  , (PACD, ICAOData (Point {pntLat = 55.21666666666667, pntLon = -162.73333333333332, pntEle = Just 30.0, pntTime = Nothing}))
  , (PACP, ICAOData (Point {pntLat = 59.8, pntLon = -144.6, pntEle = Just 18.0, pntTime = Nothing}))
  , (PACV, ICAOData (Point {pntLat = 60.5, pntLon = -145.48333333333332, pntEle = Just 13.0, pntTime = Nothing}))
  , (PACY, ICAOData (Point {pntLat = 60.083333333333336, pntLon = -142.5, pntEle = Just 4.0, pntTime = Nothing}))
  , (PACZ, ICAOData (Point {pntLat = 61.78333333333333, pntLon = -166.03333333333333, pntEle = Just 139.0, pntTime = Nothing}))
  , (PADK, ICAOData (Point {pntLat = 51.86666666666667, pntLon = -176.63333333333333, pntEle = Just 6.0, pntTime = Nothing}))
  , (PADL, ICAOData (Point {pntLat = 59.05, pntLon = -158.51666666666668, pntEle = Just 26.0, pntTime = Nothing}))
  , (PADQ, ICAOData (Point {pntLat = 57.75, pntLon = -152.48333333333332, pntEle = Just 22.0, pntTime = Nothing}))
  , (PADU, ICAOData (Point {pntLat = 53.9, pntLon = -166.53333333333333, pntEle = Just 7.0, pntTime = Nothing}))
  , (PAED, ICAOData (Point {pntLat = 61.25, pntLon = -149.78333333333333, pntEle = Just 65.0, pntTime = Nothing}))
  , (PAEH, ICAOData (Point {pntLat = 58.65, pntLon = -162.06666666666666, pntEle = Just 165.0, pntTime = Nothing}))
  , (PAEI, ICAOData (Point {pntLat = 64.65, pntLon = -147.1, pntEle = Just 167.0, pntTime = Nothing}))
  , (PAEN, ICAOData (Point {pntLat = 60.583333333333336, pntLon = -151.23333333333332, pntEle = Just 28.0, pntTime = Nothing}))
  , (PAFA, ICAOData (Point {pntLat = 64.8, pntLon = -147.86666666666667, pntEle = Just 132.0, pntTime = Nothing}))
  , (PAFM, ICAOData (Point {pntLat = 67.1, pntLon = -157.85, pntEle = Just 88.0, pntTime = Nothing}))
  , (PAFR, ICAOData (Point {pntLat = 61.266666666666666, pntLon = -149.65, pntEle = Just 115.0, pntTime = Nothing}))
  , (PAGA, ICAOData (Point {pntLat = 64.73333333333333, pntLon = -156.93333333333334, pntEle = Just 46.0, pntTime = Nothing}))
  , (PAGK, ICAOData (Point {pntLat = 62.15, pntLon = -145.45, pntEle = Just 481.0, pntTime = Nothing}))
  , (PAGM, ICAOData (Point {pntLat = 63.766666666666666, pntLon = -171.73333333333332, pntEle = Just 8.0, pntTime = Nothing}))
  , (PAGN, ICAOData (Point {pntLat = 57.5, pntLon = -134.58333333333334, pntEle = Just 0.0, pntTime = Nothing}))
  , (PAGS, ICAOData (Point {pntLat = 58.416666666666664, pntLon = -135.7, pntEle = Just 10.0, pntTime = Nothing}))
  , (PAGY, ICAOData (Point {pntLat = 59.46666666666667, pntLon = -135.31666666666666, pntEle = Just 13.0, pntTime = Nothing}))
  , (PAHD, ICAOData (Point {pntLat = 59.63333333333333, pntLon = -151.5, pntEle = Just 24.0, pntTime = Nothing}))
  , (PAIL, ICAOData (Point {pntLat = 59.75, pntLon = -154.91666666666666, pntEle = Just 63.0, pntTime = Nothing}))
  , (PAIM, ICAOData (Point {pntLat = 66.98333333333333, pntLon = -153.7, pntEle = Just 372.0, pntTime = Nothing}))
  , (PAJN, ICAOData (Point {pntLat = 58.35, pntLon = -134.56666666666666, pntEle = Just 6.0, pntTime = Nothing}))
  , (PAKN, ICAOData (Point {pntLat = 58.68333333333333, pntLon = -156.65, pntEle = Just 17.0, pntTime = Nothing}))
  , (PAKO, ICAOData (Point {pntLat = 52.95, pntLon = -168.85, pntEle = Just 21.0, pntTime = Nothing}))
  , (PAKT, ICAOData (Point {pntLat = 55.35, pntLon = -131.71666666666667, pntEle = Just 27.0, pntTime = Nothing}))
  , (PAKW, ICAOData (Point {pntLat = 55.583333333333336, pntLon = -133.08333333333334, pntEle = Just 24.0, pntTime = Nothing}))
  , (PALU, ICAOData (Point {pntLat = 68.88333333333334, pntLon = -166.1, pntEle = Just 4.0, pntTime = Nothing}))
  , (PAMC, ICAOData (Point {pntLat = 62.95, pntLon = -155.6, pntEle = Just 103.0, pntTime = Nothing}))
  , (PAMD, ICAOData (Point {pntLat = 59.43333333333333, pntLon = -146.3, pntEle = Just 27.0, pntTime = Nothing}))
  , (PAMH, ICAOData (Point {pntLat = 63.86666666666667, pntLon = -152.3, pntEle = Just 208.0, pntTime = Nothing}))
  , (PANC, ICAOData (Point {pntLat = 61.166666666666664, pntLon = -150.03333333333333, pntEle = Just 44.0, pntTime = Nothing}))
  , (PANI, ICAOData (Point {pntLat = 61.56666666666667, pntLon = -159.53333333333333, pntEle = Just 27.0, pntTime = Nothing}))
  , (PANN, ICAOData (Point {pntLat = 64.55, pntLon = -149.06666666666666, pntEle = Just 110.0, pntTime = Nothing}))
  , (PANT, ICAOData (Point {pntLat = 55.03333333333333, pntLon = -131.58333333333334, pntEle = Just 36.0, pntTime = Nothing}))
  , (PAOM, ICAOData (Point {pntLat = 64.51666666666667, pntLon = -165.43333333333334, pntEle = Just 11.0, pntTime = Nothing}))
  , (PAOR, ICAOData (Point {pntLat = 62.96666666666667, pntLon = -141.93333333333334, pntEle = Just 523.0, pntTime = Nothing}))
  , (PAOT, ICAOData (Point {pntLat = 66.88333333333334, pntLon = -162.6, pntEle = Just 3.0, pntTime = Nothing}))
  , (PAPC, ICAOData (Point {pntLat = 65.25, pntLon = -166.85, pntEle = Just 3.0, pntTime = Nothing}))
  , (PAPG, ICAOData (Point {pntLat = 56.81666666666667, pntLon = -132.96666666666667, pntEle = Just 0.0, pntTime = Nothing}))
  , (PAPH, ICAOData (Point {pntLat = 56.95, pntLon = -158.63333333333333, pntEle = Just 29.0, pntTime = Nothing}))
  , (PAPT, ICAOData (Point {pntLat = 62.1, pntLon = -152.75, pntEle = Just 560.0, pntTime = Nothing}))
  , (PAQT, ICAOData (Point {pntLat = 70.21666666666667, pntLon = -150.98333333333332, pntEle = Just 12.0, pntTime = Nothing}))
  , (PASI, ICAOData (Point {pntLat = 57.05, pntLon = -135.36666666666667, pntEle = Just 6.0, pntTime = Nothing}))
  , (PASN, ICAOData (Point {pntLat = 57.166666666666664, pntLon = -170.21666666666667, pntEle = Just 19.0, pntTime = Nothing}))
  , (PASV, ICAOData (Point {pntLat = 61.1, pntLon = -155.56666666666666, pntEle = Just 484.0, pntTime = Nothing}))
  , (PASW, ICAOData (Point {pntLat = 61.96666666666667, pntLon = -151.21666666666667, pntEle = Just 46.0, pntTime = Nothing}))
  , (PASY, ICAOData (Point {pntLat = 52.71666666666667, pntLon = 174.11666666666667, pntEle = Just 30.0, pntTime = Nothing}))
  , (PATA, ICAOData (Point {pntLat = 65.16666666666667, pntLon = -152.1, pntEle = Just 72.0, pntTime = Nothing}))
  , (PATC, ICAOData (Point {pntLat = 65.56666666666666, pntLon = -167.91666666666666, pntEle = Just 83.0, pntTime = Nothing}))
  , (PATK, ICAOData (Point {pntLat = 62.31666666666667, pntLon = -150.1, pntEle = Just 109.0, pntTime = Nothing}))
  , (PAUM, ICAOData (Point {pntLat = 69.36666666666666, pntLon = -152.13333333333333, pntEle = Just 81.0, pntTime = Nothing}))
  , (PAUN, ICAOData (Point {pntLat = 63.88333333333333, pntLon = -160.8, pntEle = Just 6.0, pntTime = Nothing}))
  , (PAWD, ICAOData (Point {pntLat = 60.11666666666667, pntLon = -149.45, pntEle = Just 9.0, pntTime = Nothing}))
  , (PAWG, ICAOData (Point {pntLat = 56.483333333333334, pntLon = -132.36666666666667, pntEle = Just 13.0, pntTime = Nothing}))
  , (PAWI, ICAOData (Point {pntLat = 70.63333333333334, pntLon = -160.03333333333333, pntEle = Just 9.0, pntTime = Nothing}))
  , (PAYA, ICAOData (Point {pntLat = 59.5, pntLon = -139.66666666666666, pntEle = Just 10.0, pntTime = Nothing}))
  , (PFYU, ICAOData (Point {pntLat = 66.56666666666666, pntLon = -145.26666666666668, pntEle = Just 433.0, pntTime = Nothing}))
  , (PGAC, ICAOData (Point {pntLat = 13.916666666666666, pntLon = 144.83333333333334, pntEle = Just 111.0, pntTime = Nothing}))
  , (PGRO, ICAOData (Point {pntLat = 14.183333333333334, pntLon = 145.25, pntEle = Just 185.0, pntTime = Nothing}))
  , (PGSN, ICAOData (Point {pntLat = 15.116666666666667, pntLon = 145.73333333333332, pntEle = Just 66.0, pntTime = Nothing}))
  , (PGUA, ICAOData (Point {pntLat = 13.583333333333334, pntLon = 144.93333333333334, pntEle = Just 187.0, pntTime = Nothing}))
  , (PGUM, ICAOData (Point {pntLat = 13.483333333333333, pntLon = 144.8, pntEle = Just 91.0, pntTime = Nothing}))
  , (PGWT, ICAOData (Point {pntLat = 14.983333333333333, pntLon = 145.61666666666667, pntEle = Just 82.0, pntTime = Nothing}))
  , (PHBK, ICAOData (Point {pntLat = 22.033333333333335, pntLon = -159.78333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (PHHI, ICAOData (Point {pntLat = 21.483333333333334, pntLon = -158.03333333333333, pntEle = Just 255.0, pntTime = Nothing}))
  , (PHJR, ICAOData (Point {pntLat = 21.316666666666666, pntLon = -158.06666666666666, pntEle = Just 10.0, pntTime = Nothing}))
  , (PHLI, ICAOData (Point {pntLat = 21.983333333333334, pntLon = -159.33333333333334, pntEle = Just 45.0, pntTime = Nothing}))
  , (PHMK, ICAOData (Point {pntLat = 21.15, pntLon = -157.1, pntEle = Just 138.0, pntTime = Nothing}))
  , (PHNG, ICAOData (Point {pntLat = 21.45, pntLon = -157.75, pntEle = Just 7.0, pntTime = Nothing}))
  , (PHNL, ICAOData (Point {pntLat = 21.333333333333332, pntLon = -157.95, pntEle = Just 4.0, pntTime = Nothing}))
  , (PHOG, ICAOData (Point {pntLat = 20.9, pntLon = -156.43333333333334, pntEle = Just 16.0, pntTime = Nothing}))
  , (PHTO, ICAOData (Point {pntLat = 19.716666666666665, pntLon = -155.05, pntEle = Just 12.0, pntTime = Nothing}))
  , (PHWH, ICAOData (Point {pntLat = 19.1, pntLon = -155.75, pntEle = Just 808.0, pntTime = Nothing}))
  , (PJON, ICAOData (Point {pntLat = 16.733333333333334, pntLon = -169.53333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (PKMJ, ICAOData (Point {pntLat = 7.066666666666666, pntLon = 171.28333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (PKWA, ICAOData (Point {pntLat = 8.716666666666667, pntLon = 167.73333333333332, pntEle = Just 3.0, pntTime = Nothing}))
  , (PLCH, ICAOData (Point {pntLat = 1.9833333333333334, pntLon = -157.48333333333332, pntEle = Just 3.0, pntTime = Nothing}))
  , (PLFA, ICAOData (Point {pntLat = 3.85, pntLon = -159.36666666666667, pntEle = Just 5.0, pntTime = Nothing}))
  , (PMDY, ICAOData (Point {pntLat = 28.216666666666665, pntLon = -177.36666666666667, pntEle = Just 4.0, pntTime = Nothing}))
  , (POLI, ICAOData (Point {pntLat = 70.5, pntLon = -149.88333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (PPIZ, ICAOData (Point {pntLat = 69.71666666666667, pntLon = -163.0, pntEle = Just 8.0, pntTime = Nothing}))
  , (PTKK, ICAOData (Point {pntLat = 7.466666666666667, pntLon = 151.85, pntEle = Just 3.0, pntTime = Nothing}))
  , (PTPN, ICAOData (Point {pntLat = 6.983333333333333, pntLon = 158.2, pntEle = Just 2.0, pntTime = Nothing}))
  , (PTRO, ICAOData (Point {pntLat = 7.366666666666666, pntLon = 134.53333333333333, pntEle = Just 54.0, pntTime = Nothing}))
  , (PTSA, ICAOData (Point {pntLat = 5.35, pntLon = 162.95, pntEle = Just 3.0, pntTime = Nothing}))
  , (PTTK, ICAOData (Point {pntLat = 5.35, pntLon = 162.95, pntEle = Just 2.0, pntTime = Nothing}))
  , (PTYA, ICAOData (Point {pntLat = 9.483333333333333, pntLon = 138.08333333333334, pntEle = Just 28.0, pntTime = Nothing}))
  , (PWAK, ICAOData (Point {pntLat = 19.283333333333335, pntLon = 166.65, pntEle = Just 4.0, pntTime = Nothing}))
  , (RCAY, ICAOData (Point {pntLat = 22.783333333333335, pntLon = 120.26666666666667, pntEle = Just 10.0, pntTime = Nothing}))
  , (RCBS, ICAOData (Point {pntLat = 24.433333333333334, pntLon = 118.36666666666666, pntEle = Just 9.0, pntTime = Nothing}))
  , (RCDC, ICAOData (Point {pntLat = 22.683333333333334, pntLon = 120.46666666666667, pntEle = Just 24.0, pntTime = Nothing}))
  , (RCFG, ICAOData (Point {pntLat = 26.166666666666668, pntLon = 119.93333333333334, pntEle = Just 91.0, pntTime = Nothing}))
  , (RCFS, ICAOData (Point {pntLat = 22.416666666666668, pntLon = 120.55, pntEle = Just 20.0, pntTime = Nothing}))
  , (RCGM, ICAOData (Point {pntLat = 25.066666666666666, pntLon = 121.23333333333333, pntEle = Just 45.0, pntTime = Nothing}))
  , (RCKH, ICAOData (Point {pntLat = 22.583333333333332, pntLon = 120.35, pntEle = Just 9.0, pntTime = Nothing}))
  , (RCKU, ICAOData (Point {pntLat = 23.466666666666665, pntLon = 120.38333333333334, pntEle = Just 25.0, pntTime = Nothing}))
  , (RCKW, ICAOData (Point {pntLat = 21.933333333333334, pntLon = 120.83333333333333, pntEle = Just 13.0, pntTime = Nothing}))
  , (RCLG, ICAOData (Point {pntLat = 24.183333333333334, pntLon = 120.65, pntEle = Just 112.0, pntTime = Nothing}))
  , (RCLM, ICAOData (Point {pntLat = 20.666666666666668, pntLon = 116.71666666666667, pntEle = Just 6.0, pntTime = Nothing}))
  , (RCLY, ICAOData (Point {pntLat = 22.033333333333335, pntLon = 121.55, pntEle = Just 325.0, pntTime = Nothing}))
  , (RCMJ, ICAOData (Point {pntLat = 22.466666666666665, pntLon = 120.43333333333334, pntEle = Just 8.0, pntTime = Nothing}))
  , (RCMQ, ICAOData (Point {pntLat = 24.266666666666666, pntLon = 120.61666666666666, pntEle = Just 5.0, pntTime = Nothing}))
  , (RCMS, ICAOData (Point {pntLat = 24.75, pntLon = 121.76666666666667, pntEle = Just 9.0, pntTime = Nothing}))
  , (RCNN, ICAOData (Point {pntLat = 22.95, pntLon = 120.2, pntEle = Just 19.0, pntTime = Nothing}))
  , (RCNO, ICAOData (Point {pntLat = 23.266666666666666, pntLon = 119.66666666666667, pntEle = Just 45.0, pntTime = Nothing}))
  , (RCPO, ICAOData (Point {pntLat = 24.816666666666666, pntLon = 120.93333333333334, pntEle = Just 8.0, pntTime = Nothing}))
  , (RCQC, ICAOData (Point {pntLat = 23.583333333333332, pntLon = 119.61666666666666, pntEle = Just 31.0, pntTime = Nothing}))
  , (RCQS, ICAOData (Point {pntLat = 22.8, pntLon = 121.18333333333334, pntEle = Just 37.0, pntTime = Nothing}))
  , (RCSQ, ICAOData (Point {pntLat = 22.7, pntLon = 120.48333333333333, pntEle = Just 29.0, pntTime = Nothing}))
  , (RCSS, ICAOData (Point {pntLat = 25.066666666666666, pntLon = 121.55, pntEle = Just 6.0, pntTime = Nothing}))
  , (RCTP, ICAOData (Point {pntLat = 25.083333333333332, pntLon = 121.21666666666667, pntEle = Just 33.0, pntTime = Nothing}))
  , (RCUK, ICAOData (Point {pntLat = 24.933333333333334, pntLon = 121.3, pntEle = Just 141.0, pntTime = Nothing}))
  , (RCYU, ICAOData (Point {pntLat = 24.033333333333335, pntLon = 121.61666666666666, pntEle = Just 16.0, pntTime = Nothing}))
  , (RJAA, ICAOData (Point {pntLat = 35.766666666666666, pntLon = 140.38333333333333, pntEle = Just 41.0, pntTime = Nothing}))
  , (RJAF, ICAOData (Point {pntLat = 36.166666666666664, pntLon = 137.93333333333334, pntEle = Just 668.0, pntTime = Nothing}))
  , (RJAH, ICAOData (Point {pntLat = 36.18333333333333, pntLon = 140.41666666666666, pntEle = Just 32.0, pntTime = Nothing}))
  , (RJAK, ICAOData (Point {pntLat = 36.03333333333333, pntLon = 140.2, pntEle = Just 26.0, pntTime = Nothing}))
  , (RJAM, ICAOData (Point {pntLat = 24.3, pntLon = 153.96666666666667, pntEle = Just 8.0, pntTime = Nothing}))
  , (RJAO, ICAOData (Point {pntLat = 27.083333333333332, pntLon = 142.18333333333334, pntEle = Just 3.0, pntTime = Nothing}))
  , (RJAT, ICAOData (Point {pntLat = 35.31666666666667, pntLon = 138.86666666666667, pntEle = Just 680.0, pntTime = Nothing}))
  , (RJAW, ICAOData (Point {pntLat = 24.783333333333335, pntLon = 141.31666666666666, pntEle = Just 113.0, pntTime = Nothing}))
  , (RJBB, ICAOData (Point {pntLat = 34.416666666666664, pntLon = 135.25, pntEle = Just 5.0, pntTime = Nothing}))
  , (RJBD, ICAOData (Point {pntLat = 33.666666666666664, pntLon = 135.35, pntEle = Just 105.0, pntTime = Nothing}))
  , (RJCA, ICAOData (Point {pntLat = 43.8, pntLon = 142.36666666666667, pntEle = Just 115.0, pntTime = Nothing}))
  , (RJCB, ICAOData (Point {pntLat = 42.733333333333334, pntLon = 143.21666666666667, pntEle = Just 149.0, pntTime = Nothing}))
  , (RJCC, ICAOData (Point {pntLat = 42.8, pntLon = 141.66666666666666, pntEle = Just 27.0, pntTime = Nothing}))
  , (RJCH, ICAOData (Point {pntLat = 41.766666666666666, pntLon = 140.81666666666666, pntEle = Just 33.0, pntTime = Nothing}))
  , (RJCJ, ICAOData (Point {pntLat = 42.81666666666667, pntLon = 141.68333333333334, pntEle = Just 27.0, pntTime = Nothing}))
  , (RJCK, ICAOData (Point {pntLat = 43.03333333333333, pntLon = 144.2, pntEle = Just 95.0, pntTime = Nothing}))
  , (RJCM, ICAOData (Point {pntLat = 43.88333333333333, pntLon = 144.16666666666666, pntEle = Just 33.0, pntTime = Nothing}))
  , (RJCN, ICAOData (Point {pntLat = 43.56666666666667, pntLon = 144.96666666666667, pntEle = Just 66.0, pntTime = Nothing}))
  , (RJCO, ICAOData (Point {pntLat = 43.11666666666667, pntLon = 141.38333333333333, pntEle = Just 8.0, pntTime = Nothing}))
  , (RJCS, ICAOData (Point {pntLat = 42.983333333333334, pntLon = 144.4, pntEle = Just 32.0, pntTime = Nothing}))
  , (RJCW, ICAOData (Point {pntLat = 45.4, pntLon = 141.8, pntEle = Just 8.0, pntTime = Nothing}))
  , (RJCY, ICAOData (Point {pntLat = 42.31666666666667, pntLon = 140.98333333333332, pntEle = Just 40.0, pntTime = Nothing}))
  , (RJDB, ICAOData (Point {pntLat = 33.75, pntLon = 129.78333333333333, pntEle = Just 13.0, pntTime = Nothing}))
  , (RJDC, ICAOData (Point {pntLat = 33.93333333333333, pntLon = 131.28333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (RJDM, ICAOData (Point {pntLat = 33.31666666666667, pntLon = 130.41666666666666, pntEle = Just 16.0, pntTime = Nothing}))
  , (RJDT, ICAOData (Point {pntLat = 34.28333333333333, pntLon = 129.33333333333334, pntEle = Just 63.0, pntTime = Nothing}))
  , (RJEB, ICAOData (Point {pntLat = 44.25, pntLon = 143.53333333333333, pntEle = Just 6.0, pntTime = Nothing}))
  , (RJEC, ICAOData (Point {pntLat = 43.666666666666664, pntLon = 142.45, pntEle = Just 208.0, pntTime = Nothing}))
  , (RJFA, ICAOData (Point {pntLat = 33.88333333333333, pntLon = 130.65, pntEle = Just 30.0, pntTime = Nothing}))
  , (RJFC, ICAOData (Point {pntLat = 30.383333333333333, pntLon = 130.66666666666666, pntEle = Just 36.0, pntTime = Nothing}))
  , (RJFE, ICAOData (Point {pntLat = 32.666666666666664, pntLon = 128.83333333333334, pntEle = Just 77.0, pntTime = Nothing}))
  , (RJFF, ICAOData (Point {pntLat = 33.583333333333336, pntLon = 130.45, pntEle = Just 9.0, pntTime = Nothing}))
  , (RJFG, ICAOData (Point {pntLat = 30.55, pntLon = 130.95, pntEle = Just 92.0, pntTime = Nothing}))
  , (RJFK, ICAOData (Point {pntLat = 31.8, pntLon = 130.71666666666667, pntEle = Just 272.0, pntTime = Nothing}))
  , (RJFM, ICAOData (Point {pntLat = 31.866666666666667, pntLon = 131.45, pntEle = Just 6.0, pntTime = Nothing}))
  , (RJFN, ICAOData (Point {pntLat = 32.083333333333336, pntLon = 131.45, pntEle = Just 79.0, pntTime = Nothing}))
  , (RJFO, ICAOData (Point {pntLat = 33.483333333333334, pntLon = 131.73333333333332, pntEle = Just 5.0, pntTime = Nothing}))
  , (RJFR, ICAOData (Point {pntLat = 33.833333333333336, pntLon = 130.95, pntEle = Just 3.0, pntTime = Nothing}))
  , (RJFT, ICAOData (Point {pntLat = 32.833333333333336, pntLon = 130.85, pntEle = Just 193.0, pntTime = Nothing}))
  , (RJFU, ICAOData (Point {pntLat = 32.916666666666664, pntLon = 129.91666666666666, pntEle = Just 2.0, pntTime = Nothing}))
  , (RJFW, ICAOData (Point {pntLat = 33.15, pntLon = 129.71666666666667, pntEle = Just 19.0, pntTime = Nothing}))
  , (RJFY, ICAOData (Point {pntLat = 31.366666666666667, pntLon = 130.83333333333334, pntEle = Just 65.0, pntTime = Nothing}))
  , (RJFZ, ICAOData (Point {pntLat = 33.68333333333333, pntLon = 131.05, pntEle = Just 17.0, pntTime = Nothing}))
  , (RJKA, ICAOData (Point {pntLat = 28.433333333333334, pntLon = 129.71666666666667, pntEle = Just 4.0, pntTime = Nothing}))
  , (RJKB, ICAOData (Point {pntLat = 27.433333333333334, pntLon = 128.7, pntEle = Just 27.0, pntTime = Nothing}))
  , (RJKN, ICAOData (Point {pntLat = 27.833333333333332, pntLon = 128.88333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (RJNF, ICAOData (Point {pntLat = 36.13333333333333, pntLon = 136.23333333333332, pntEle = Just 5.0, pntTime = Nothing}))
  , (RJNG, ICAOData (Point {pntLat = 35.38333333333333, pntLon = 136.86666666666667, pntEle = Just 39.0, pntTime = Nothing}))
  , (RJNH, ICAOData (Point {pntLat = 34.75, pntLon = 137.7, pntEle = Just 45.0, pntTime = Nothing}))
  , (RJNK, ICAOData (Point {pntLat = 36.38333333333333, pntLon = 136.41666666666666, pntEle = Just 6.0, pntTime = Nothing}))
  , (RJNN, ICAOData (Point {pntLat = 35.25, pntLon = 136.93333333333334, pntEle = Just 14.0, pntTime = Nothing}))
  , (RJNO, ICAOData (Point {pntLat = 36.18333333333333, pntLon = 133.33333333333334, pntEle = Just 92.0, pntTime = Nothing}))
  , (RJNT, ICAOData (Point {pntLat = 36.65, pntLon = 137.18333333333334, pntEle = Just 24.0, pntTime = Nothing}))
  , (RJNY, ICAOData (Point {pntLat = 34.81666666666667, pntLon = 138.3, pntEle = Just 7.0, pntTime = Nothing}))
  , (RJOA, ICAOData (Point {pntLat = 34.43333333333333, pntLon = 132.91666666666666, pntEle = Just 331.0, pntTime = Nothing}))
  , (RJOB, ICAOData (Point {pntLat = 34.75, pntLon = 133.85, pntEle = Just 241.0, pntTime = Nothing}))
  , (RJOC, ICAOData (Point {pntLat = 35.416666666666664, pntLon = 132.9, pntEle = Just 2.0, pntTime = Nothing}))
  , (RJOE, ICAOData (Point {pntLat = 34.53333333333333, pntLon = 136.68333333333334, pntEle = Just 6.0, pntTime = Nothing}))
  , (RJOF, ICAOData (Point {pntLat = 34.03333333333333, pntLon = 131.55, pntEle = Just 2.0, pntTime = Nothing}))
  , (RJOH, ICAOData (Point {pntLat = 35.483333333333334, pntLon = 133.25, pntEle = Just 6.0, pntTime = Nothing}))
  , (RJOI, ICAOData (Point {pntLat = 34.15, pntLon = 132.23333333333332, pntEle = Just 2.0, pntTime = Nothing}))
  , (RJOK, ICAOData (Point {pntLat = 33.53333333333333, pntLon = 133.66666666666666, pntEle = Just 7.0, pntTime = Nothing}))
  , (RJOM, ICAOData (Point {pntLat = 33.81666666666667, pntLon = 132.7, pntEle = Just 4.0, pntTime = Nothing}))
  , (RJOO, ICAOData (Point {pntLat = 34.78333333333333, pntLon = 135.45, pntEle = Just 12.0, pntTime = Nothing}))
  , (RJOP, ICAOData (Point {pntLat = 34.0, pntLon = 134.63333333333333, pntEle = Just 3.0, pntTime = Nothing}))
  , (RJOR, ICAOData (Point {pntLat = 35.53333333333333, pntLon = 134.16666666666666, pntEle = Just 15.0, pntTime = Nothing}))
  , (RJOS, ICAOData (Point {pntLat = 34.13333333333333, pntLon = 134.61666666666667, pntEle = Just 8.0, pntTime = Nothing}))
  , (RJOT, ICAOData (Point {pntLat = 34.21666666666667, pntLon = 134.01666666666668, pntEle = Just 185.0, pntTime = Nothing}))
  , (RJOW, ICAOData (Point {pntLat = 34.666666666666664, pntLon = 131.8, pntEle = Just 54.0, pntTime = Nothing}))
  , (RJOY, ICAOData (Point {pntLat = 34.6, pntLon = 135.6, pntEle = Just 10.0, pntTime = Nothing}))
  , (RJOZ, ICAOData (Point {pntLat = 34.05, pntLon = 131.05, pntEle = Just 4.0, pntTime = Nothing}))
  , (RJSA, ICAOData (Point {pntLat = 40.733333333333334, pntLon = 140.7, pntEle = Just 199.0, pntTime = Nothing}))
  , (RJSC, ICAOData (Point {pntLat = 38.416666666666664, pntLon = 140.36666666666667, pntEle = Just 105.0, pntTime = Nothing}))
  , (RJSD, ICAOData (Point {pntLat = 38.05, pntLon = 138.41666666666666, pntEle = Just 23.0, pntTime = Nothing}))
  , (RJSF, ICAOData (Point {pntLat = 37.233333333333334, pntLon = 140.43333333333334, pntEle = Just 372.0, pntTime = Nothing}))
  , (RJSH, ICAOData (Point {pntLat = 40.55, pntLon = 141.46666666666667, pntEle = Just 46.0, pntTime = Nothing}))
  , (RJSI, ICAOData (Point {pntLat = 39.43333333333333, pntLon = 141.13333333333333, pntEle = Just 90.0, pntTime = Nothing}))
  , (RJSK, ICAOData (Point {pntLat = 39.61666666666667, pntLon = 140.21666666666667, pntEle = Just 93.0, pntTime = Nothing}))
  , (RJSM, ICAOData (Point {pntLat = 40.7, pntLon = 141.38333333333333, pntEle = Just 36.0, pntTime = Nothing}))
  , (RJSN, ICAOData (Point {pntLat = 37.95, pntLon = 139.11666666666667, pntEle = Just 1.0, pntTime = Nothing}))
  , (RJSO, ICAOData (Point {pntLat = 41.233333333333334, pntLon = 141.13333333333333, pntEle = Just 7.0, pntTime = Nothing}))
  , (RJSS, ICAOData (Point {pntLat = 38.13333333333333, pntLon = 140.91666666666666, pntEle = Just 1.0, pntTime = Nothing}))
  , (RJST, ICAOData (Point {pntLat = 38.4, pntLon = 141.21666666666667, pntEle = Just 2.0, pntTime = Nothing}))
  , (RJSU, ICAOData (Point {pntLat = 38.233333333333334, pntLon = 140.91666666666666, pntEle = Just 7.0, pntTime = Nothing}))
  , (RJTA, ICAOData (Point {pntLat = 35.45, pntLon = 139.45, pntEle = Just 62.0, pntTime = Nothing}))
  , (RJTC, ICAOData (Point {pntLat = 35.7, pntLon = 139.4, pntEle = Just 95.0, pntTime = Nothing}))
  , (RJTD, ICAOData (Point {pntLat = 35.68333333333333, pntLon = 139.76666666666668, pntEle = Just 5.0, pntTime = Nothing}))
  , (RJTE, ICAOData (Point {pntLat = 34.983333333333334, pntLon = 139.83333333333334, pntEle = Just 3.0, pntTime = Nothing}))
  , (RJTF, ICAOData (Point {pntLat = 35.666666666666664, pntLon = 139.53333333333333, pntEle = Just 41.0, pntTime = Nothing}))
  , (RJTH, ICAOData (Point {pntLat = 33.11666666666667, pntLon = 139.78333333333333, pntEle = Just 92.0, pntTime = Nothing}))
  , (RJTI, ICAOData (Point {pntLat = 35.63333333333333, pntLon = 139.85, pntEle = Just 5.0, pntTime = Nothing}))
  , (RJTJ, ICAOData (Point {pntLat = 35.833333333333336, pntLon = 139.41666666666666, pntEle = Just 90.0, pntTime = Nothing}))
  , (RJTK, ICAOData (Point {pntLat = 35.4, pntLon = 139.91666666666666, pntEle = Just 3.0, pntTime = Nothing}))
  , (RJTL, ICAOData (Point {pntLat = 35.8, pntLon = 140.01666666666668, pntEle = Just 30.0, pntTime = Nothing}))
  , (RJTO, ICAOData (Point {pntLat = 34.78333333333333, pntLon = 139.36666666666667, pntEle = Just 38.0, pntTime = Nothing}))
  , (RJTQ, ICAOData (Point {pntLat = 34.06666666666667, pntLon = 139.56666666666666, pntEle = Just 20.0, pntTime = Nothing}))
  , (RJTR, ICAOData (Point {pntLat = 35.516666666666666, pntLon = 139.4, pntEle = Just 109.0, pntTime = Nothing}))
  , (RJTT, ICAOData (Point {pntLat = 35.55, pntLon = 139.78333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (RJTU, ICAOData (Point {pntLat = 36.516666666666666, pntLon = 139.86666666666667, pntEle = Just 102.0, pntTime = Nothing}))
  , (RJTX, ICAOData (Point {pntLat = 35.28333333333333, pntLon = 139.66666666666666, pntEle = Just 49.0, pntTime = Nothing}))
  , (RJTY, ICAOData (Point {pntLat = 35.75, pntLon = 139.35, pntEle = Just 139.0, pntTime = Nothing}))
  , (RKJJ, ICAOData (Point {pntLat = 35.11666666666667, pntLon = 126.81666666666666, pntEle = Just 13.0, pntTime = Nothing}))
  , (RKJK, ICAOData (Point {pntLat = 35.916666666666664, pntLon = 126.61666666666666, pntEle = Just 10.0, pntTime = Nothing}))
  , (RKJY, ICAOData (Point {pntLat = 34.833333333333336, pntLon = 127.61666666666666, pntEle = Just 21.0, pntTime = Nothing}))
  , (RKNC, ICAOData (Point {pntLat = 37.86666666666667, pntLon = 127.71666666666667, pntEle = Just 76.0, pntTime = Nothing}))
  , (RKNH, ICAOData (Point {pntLat = 37.43333333333333, pntLon = 127.95, pntEle = Just 100.0, pntTime = Nothing}))
  , (RKNN, ICAOData (Point {pntLat = 37.75, pntLon = 128.95, pntEle = Just 11.0, pntTime = Nothing}))
  , (RKNW, ICAOData (Point {pntLat = 37.333333333333336, pntLon = 127.95, pntEle = Just 150.0, pntTime = Nothing}))
  , (RKPC, ICAOData (Point {pntLat = 33.5, pntLon = 126.5, pntEle = Just 36.0, pntTime = Nothing}))
  , (RKPK, ICAOData (Point {pntLat = 35.18333333333333, pntLon = 128.93333333333334, pntEle = Just 4.0, pntTime = Nothing}))
  , (RKPM, ICAOData (Point {pntLat = 33.2, pntLon = 126.26666666666667, pntEle = Just 27.0, pntTime = Nothing}))
  , (RKPS, ICAOData (Point {pntLat = 35.083333333333336, pntLon = 128.08333333333334, pntEle = Just 8.0, pntTime = Nothing}))
  , (RKPU, ICAOData (Point {pntLat = 35.55, pntLon = 129.31666666666666, pntEle = Just 32.0, pntTime = Nothing}))
  , (RKSB, ICAOData (Point {pntLat = 37.5, pntLon = 129.13333333333333, pntEle = Just 31.0, pntTime = Nothing}))
  , (RKSF, ICAOData (Point {pntLat = 37.5, pntLon = 126.93333333333334, pntEle = Just 49.0, pntTime = Nothing}))
  , (RKSG, ICAOData (Point {pntLat = 36.93333333333333, pntLon = 127.0, pntEle = Just 16.0, pntTime = Nothing}))
  , (RKSI, ICAOData (Point {pntLat = 37.46666666666667, pntLon = 126.45, pntEle = Just 7.0, pntTime = Nothing}))
  , (RKSL, ICAOData (Point {pntLat = 37.56666666666667, pntLon = 126.96666666666667, pntEle = Just 86.0, pntTime = Nothing}))
  , (RKSM, ICAOData (Point {pntLat = 37.43333333333333, pntLon = 127.11666666666666, pntEle = Just 20.0, pntTime = Nothing}))
  , (RKSN, ICAOData (Point {pntLat = 37.03333333333333, pntLon = 126.75, pntEle = Just 15.0, pntTime = Nothing}))
  , (RKSO, ICAOData (Point {pntLat = 37.1, pntLon = 127.03333333333333, pntEle = Just 11.0, pntTime = Nothing}))
  , (RKSP, ICAOData (Point {pntLat = 37.96666666666667, pntLon = 124.66666666666667, pntEle = Just 171.0, pntTime = Nothing}))
  , (RKSS, ICAOData (Point {pntLat = 37.55, pntLon = 126.8, pntEle = Just 18.0, pntTime = Nothing}))
  , (RKSW, ICAOData (Point {pntLat = 37.25, pntLon = 127.0, pntEle = Just 24.0, pntTime = Nothing}))
  , (RKTA, ICAOData (Point {pntLat = 36.55, pntLon = 128.71666666666667, pntEle = Just 139.0, pntTime = Nothing}))
  , (RKTD, ICAOData (Point {pntLat = 36.333333333333336, pntLon = 127.38333333333334, pntEle = Just 63.0, pntTime = Nothing}))
  , (RKTE, ICAOData (Point {pntLat = 36.4, pntLon = 127.5, pntEle = Just 79.0, pntTime = Nothing}))
  , (RKTF, ICAOData (Point {pntLat = 36.3, pntLon = 127.4, pntEle = Just 77.0, pntTime = Nothing}))
  , (RKTH, ICAOData (Point {pntLat = 35.983333333333334, pntLon = 129.41666666666666, pntEle = Just 20.0, pntTime = Nothing}))
  , (RKTI, ICAOData (Point {pntLat = 37.03333333333333, pntLon = 127.88333333333334, pntEle = Just 91.0, pntTime = Nothing}))
  , (RKTM, ICAOData (Point {pntLat = 36.93333333333333, pntLon = 126.45, pntEle = Just 302.0, pntTime = Nothing}))
  , (RKTN, ICAOData (Point {pntLat = 35.9, pntLon = 128.65, pntEle = Just 35.0, pntTime = Nothing}))
  , (RKTT, ICAOData (Point {pntLat = 35.88333333333333, pntLon = 128.61666666666667, pntEle = Just 58.0, pntTime = Nothing}))
  , (RKTU, ICAOData (Point {pntLat = 36.7, pntLon = 127.5, pntEle = Just 58.0, pntTime = Nothing}))
  , (RKTY, ICAOData (Point {pntLat = 36.63333333333333, pntLon = 128.35, pntEle = Just 108.0, pntTime = Nothing}))
  , (ROAH, ICAOData (Point {pntLat = 26.183333333333334, pntLon = 127.65, pntEle = Just 3.0, pntTime = Nothing}))
  , (RODE, ICAOData (Point {pntLat = 26.716666666666665, pntLon = 127.78333333333333, pntEle = Just 70.0, pntTime = Nothing}))
  , (RODN, ICAOData (Point {pntLat = 26.35, pntLon = 127.76666666666667, pntEle = Just 45.0, pntTime = Nothing}))
  , (ROHF, ICAOData (Point {pntLat = 26.3, pntLon = 127.76666666666667, pntEle = Just 5.0, pntTime = Nothing}))
  , (ROIG, ICAOData (Point {pntLat = 24.333333333333332, pntLon = 124.16666666666667, pntEle = Just 6.0, pntTime = Nothing}))
  , (ROKJ, ICAOData (Point {pntLat = 26.333333333333332, pntLon = 126.8, pntEle = Just 4.0, pntTime = Nothing}))
  , (ROMD, ICAOData (Point {pntLat = 25.833333333333332, pntLon = 131.23333333333332, pntEle = Just 16.0, pntTime = Nothing}))
  , (ROMY, ICAOData (Point {pntLat = 24.783333333333335, pntLon = 125.28333333333333, pntEle = Just 40.0, pntTime = Nothing}))
  , (ROTM, ICAOData (Point {pntLat = 26.266666666666666, pntLon = 127.75, pntEle = Just 75.0, pntTime = Nothing}))
  , (ROYN, ICAOData (Point {pntLat = 24.466666666666665, pntLon = 122.98333333333333, pntEle = Just 16.0, pntTime = Nothing}))
  , (RPLB, ICAOData (Point {pntLat = 14.8, pntLon = 120.26666666666667, pntEle = Just 18.0, pntTime = Nothing}))
  , (RPLI, ICAOData (Point {pntLat = 18.183333333333334, pntLon = 120.53333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (RPLL, ICAOData (Point {pntLat = 14.516666666666667, pntLon = 121.0, pntEle = Just 14.0, pntTime = Nothing}))
  , (RPMD, ICAOData (Point {pntLat = 7.116666666666666, pntLon = 125.65, pntEle = Just 17.0, pntTime = Nothing}))
  , (RPMK, ICAOData (Point {pntLat = 15.166666666666666, pntLon = 120.56666666666666, pntEle = Just 196.0, pntTime = Nothing}))
  , (RPMP, ICAOData (Point {pntLat = 13.133333333333333, pntLon = 123.73333333333333, pntEle = Just 16.0, pntTime = Nothing}))
  , (RPMR, ICAOData (Point {pntLat = 12.583333333333334, pntLon = 122.26666666666667, pntEle = Just 46.0, pntTime = Nothing}))
  , (RPMS, ICAOData (Point {pntLat = 14.5, pntLon = 120.91666666666667, pntEle = Just 3.0, pntTime = Nothing}))
  , (RPMT, ICAOData (Point {pntLat = 10.3, pntLon = 123.96666666666667, pntEle = Just 23.0, pntTime = Nothing}))
  , (RPMZ, ICAOData (Point {pntLat = 6.9, pntLon = 122.06666666666666, pntEle = Just 5.0, pntTime = Nothing}))
  , (RPUA, ICAOData (Point {pntLat = 18.366666666666667, pntLon = 121.63333333333334, pntEle = Just 2.0, pntTime = Nothing}))
  , (RPUB, ICAOData (Point {pntLat = 16.416666666666668, pntLon = 120.6, pntEle = Just 1500.0, pntTime = Nothing}))
  , (RPUD, ICAOData (Point {pntLat = 14.133333333333333, pntLon = 122.98333333333333, pntEle = Just 3.0, pntTime = Nothing}))
  , (RPUH, ICAOData (Point {pntLat = 12.35, pntLon = 121.03333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (RPUI, ICAOData (Point {pntLat = 15.333333333333334, pntLon = 119.96666666666667, pntEle = Just 4.0, pntTime = Nothing}))
  , (RPUK, ICAOData (Point {pntLat = 13.416666666666666, pntLon = 121.18333333333334, pntEle = Just 39.0, pntTime = Nothing}))
  , (RPUO, ICAOData (Point {pntLat = 20.45, pntLon = 121.96666666666667, pntEle = Just 10.0, pntTime = Nothing}))
  , (RPUQ, ICAOData (Point {pntLat = 17.566666666666666, pntLon = 120.38333333333334, pntEle = Just 31.0, pntTime = Nothing}))
  , (RPUR, ICAOData (Point {pntLat = 15.766666666666667, pntLon = 121.56666666666666, pntEle = Just 4.0, pntTime = Nothing}))
  , (RPUT, ICAOData (Point {pntLat = 17.616666666666667, pntLon = 121.73333333333333, pntEle = Just 61.0, pntTime = Nothing}))
  , (RPUV, ICAOData (Point {pntLat = 13.583333333333334, pntLon = 124.23333333333333, pntEle = Just 39.0, pntTime = Nothing}))
  , (RPVA, ICAOData (Point {pntLat = 11.25, pntLon = 125.0, pntEle = Just 2.0, pntTime = Nothing}))
  , (RPVD, ICAOData (Point {pntLat = 9.3, pntLon = 123.3, pntEle = Just 5.0, pntTime = Nothing}))
  , (RPVF, ICAOData (Point {pntLat = 12.483333333333333, pntLon = 124.63333333333334, pntEle = Just 4.0, pntTime = Nothing}))
  , (RPVG, ICAOData (Point {pntLat = 11.033333333333333, pntLon = 126.73333333333333, pntEle = Just 56.0, pntTime = Nothing}))
  , (RPVI, ICAOData (Point {pntLat = 10.7, pntLon = 122.56666666666666, pntEle = Just 7.0, pntTime = Nothing}))
  , (RPVM, ICAOData (Point {pntLat = 12.366666666666667, pntLon = 123.61666666666666, pntEle = Just 6.0, pntTime = Nothing}))
  , (RPVP, ICAOData (Point {pntLat = 9.75, pntLon = 118.73333333333333, pntEle = Just 14.0, pntTime = Nothing}))
  , (RPVR, ICAOData (Point {pntLat = 11.583333333333334, pntLon = 122.75, pntEle = Just 3.0, pntTime = Nothing}))
  , (RPVT, ICAOData (Point {pntLat = 9.6, pntLon = 123.85, pntEle = Just 7.0, pntTime = Nothing}))
  , (RPWB, ICAOData (Point {pntLat = 6.116666666666666, pntLon = 125.18333333333334, pntEle = Just 14.0, pntTime = Nothing}))
  , (RPWC, ICAOData (Point {pntLat = 7.166666666666667, pntLon = 124.21666666666667, pntEle = Just 58.0, pntTime = Nothing}))
  , (RPWE, ICAOData (Point {pntLat = 9.933333333333334, pntLon = 125.51666666666667, pntEle = Just 45.0, pntTime = Nothing}))
  , (RPWG, ICAOData (Point {pntLat = 8.6, pntLon = 123.35, pntEle = Just 3.0, pntTime = Nothing}))
  , (RPWL, ICAOData (Point {pntLat = 8.483333333333333, pntLon = 124.63333333333334, pntEle = Just 5.0, pntTime = Nothing}))
  , (RPWS, ICAOData (Point {pntLat = 9.8, pntLon = 125.5, pntEle = Just 54.0, pntTime = Nothing}))
  , (RPWY, ICAOData (Point {pntLat = 8.15, pntLon = 125.08333333333333, pntEle = Just 626.0, pntTime = Nothing}))
  , (RPXT, ICAOData (Point {pntLat = 14.083333333333334, pntLon = 122.01666666666667, pntEle = Just 4.0, pntTime = Nothing}))
  , (SAAC, ICAOData (Point {pntLat = -31.3, pntLon = -58.016666666666666, pntEle = Just 35.0, pntTime = Nothing}))
  , (SAAG, ICAOData (Point {pntLat = -33.0, pntLon = -58.61666666666667, pntEle = Just 23.0, pntTime = Nothing}))
  , (SAAJ, ICAOData (Point {pntLat = -34.55, pntLon = -60.916666666666664, pntEle = Just 82.0, pntTime = Nothing}))
  , (SAAP, ICAOData (Point {pntLat = -31.783333333333335, pntLon = -60.483333333333334, pntEle = Just 74.0, pntTime = Nothing}))
  , (SAAR, ICAOData (Point {pntLat = -32.916666666666664, pntLon = -60.78333333333333, pntEle = Just 25.0, pntTime = Nothing}))
  , (SAAU, ICAOData (Point {pntLat = -31.85, pntLon = -59.083333333333336, pntEle = Just 40.0, pntTime = Nothing}))
  , (SAAV, ICAOData (Point {pntLat = -31.7, pntLon = -60.81666666666667, pntEle = Just 17.0, pntTime = Nothing}))
  , (SABA, ICAOData (Point {pntLat = -34.583333333333336, pntLon = -58.483333333333334, pntEle = Just 25.0, pntTime = Nothing}))
  , (SABE, ICAOData (Point {pntLat = -34.56666666666667, pntLon = -58.416666666666664, pntEle = Just 6.0, pntTime = Nothing}))
  , (SACI, ICAOData (Point {pntLat = -31.666666666666668, pntLon = -63.88333333333333, pntEle = Just 338.0, pntTime = Nothing}))
  , (SACO, ICAOData (Point {pntLat = -31.316666666666666, pntLon = -64.21666666666667, pntEle = Just 484.0, pntTime = Nothing}))
  , (SACP, ICAOData (Point {pntLat = -31.333333333333332, pntLon = -66.6, pntEle = Just 658.0, pntTime = Nothing}))
  , (SACV, ICAOData (Point {pntLat = -29.9, pntLon = -63.68333333333333, pntEle = Just 341.0, pntTime = Nothing}))
  , (SADD, ICAOData (Point {pntLat = -34.483333333333334, pntLon = -58.61666666666667, pntEle = Just 5.0, pntTime = Nothing}))
  , (SADL, ICAOData (Point {pntLat = -34.96666666666667, pntLon = -57.9, pntEle = Just 21.0, pntTime = Nothing}))
  , (SADP, ICAOData (Point {pntLat = -34.6, pntLon = -58.6, pntEle = Just 18.0, pntTime = Nothing}))
  , (SAEZ, ICAOData (Point {pntLat = -34.81666666666667, pntLon = -58.53333333333333, pntEle = Just 20.0, pntTime = Nothing}))
  , (SAME, ICAOData (Point {pntLat = -32.833333333333336, pntLon = -68.78333333333333, pntEle = Just 705.0, pntTime = Nothing}))
  , (SAMI, ICAOData (Point {pntLat = -33.083333333333336, pntLon = -68.41666666666667, pntEle = Just 653.0, pntTime = Nothing}))
  , (SAMJ, ICAOData (Point {pntLat = -30.25, pntLon = -68.75, pntEle = Just 1165.0, pntTime = Nothing}))
  , (SAMM, ICAOData (Point {pntLat = -35.5, pntLon = -69.58333333333333, pntEle = Just 1426.0, pntTime = Nothing}))
  , (SAMR, ICAOData (Point {pntLat = -34.583333333333336, pntLon = -68.4, pntEle = Just 745.0, pntTime = Nothing}))
  , (SAMS, ICAOData (Point {pntLat = -33.766666666666666, pntLon = -69.03333333333333, pntEle = Just 940.0, pntTime = Nothing}))
  , (SAMU, ICAOData (Point {pntLat = -32.6, pntLon = -69.33333333333333, pntEle = Just 1844.0, pntTime = Nothing}))
  , (SANC, ICAOData (Point {pntLat = -28.6, pntLon = -65.76666666666667, pntEle = Just 454.0, pntTime = Nothing}))
  , (SANE, ICAOData (Point {pntLat = -27.766666666666666, pntLon = -64.3, pntEle = Just 198.0, pntTime = Nothing}))
  , (SANI, ICAOData (Point {pntLat = -28.066666666666666, pntLon = -67.56666666666666, pntEle = Just 1201.0, pntTime = Nothing}))
  , (SANL, ICAOData (Point {pntLat = -29.383333333333333, pntLon = -66.81666666666666, pntEle = Just 438.0, pntTime = Nothing}))
  , (SANO, ICAOData (Point {pntLat = -29.216666666666665, pntLon = -67.43333333333334, pntEle = Just 950.0, pntTime = Nothing}))
  , (SANT, ICAOData (Point {pntLat = -26.85, pntLon = -65.1, pntEle = Just 440.0, pntTime = Nothing}))
  , (SANU, ICAOData (Point {pntLat = -31.566666666666666, pntLon = -68.86666666666666, pntEle = Just 597.0, pntTime = Nothing}))
  , (SANW, ICAOData (Point {pntLat = -29.883333333333333, pntLon = -61.95, pntEle = Just 87.0, pntTime = Nothing}))
  , (SAOC, ICAOData (Point {pntLat = -33.11666666666667, pntLon = -64.23333333333333, pntEle = Just 420.0, pntTime = Nothing}))
  , (SAOD, ICAOData (Point {pntLat = -31.95, pntLon = -65.13333333333334, pntEle = Just 561.0, pntTime = Nothing}))
  , (SAOL, ICAOData (Point {pntLat = -34.13333333333333, pntLon = -63.36666666666667, pntEle = Just 136.0, pntTime = Nothing}))
  , (SAOM, ICAOData (Point {pntLat = -32.7, pntLon = -62.15, pntEle = Just 110.0, pntTime = Nothing}))
  , (SAOR, ICAOData (Point {pntLat = -33.733333333333334, pntLon = -65.38333333333334, pntEle = Just 485.0, pntTime = Nothing}))
  , (SAOU, ICAOData (Point {pntLat = -33.266666666666666, pntLon = -66.35, pntEle = Just 710.0, pntTime = Nothing}))
  , (SARC, ICAOData (Point {pntLat = -27.45, pntLon = -58.766666666666666, pntEle = Just 62.0, pntTime = Nothing}))
  , (SARE, ICAOData (Point {pntLat = -27.45, pntLon = -59.05, pntEle = Just 53.0, pntTime = Nothing}))
  , (SARF, ICAOData (Point {pntLat = -26.2, pntLon = -58.233333333333334, pntEle = Just 59.0, pntTime = Nothing}))
  , (SARI, ICAOData (Point {pntLat = -25.733333333333334, pntLon = -54.46666666666667, pntEle = Just 270.0, pntTime = Nothing}))
  , (SARL, ICAOData (Point {pntLat = -29.683333333333334, pntLon = -57.15, pntEle = Just 69.0, pntTime = Nothing}))
  , (SARM, ICAOData (Point {pntLat = -30.266666666666666, pntLon = -57.65, pntEle = Just 53.0, pntTime = Nothing}))
  , (SARP, ICAOData (Point {pntLat = -27.366666666666667, pntLon = -55.96666666666667, pntEle = Just 131.0, pntTime = Nothing}))
  , (SARS, ICAOData (Point {pntLat = -26.816666666666666, pntLon = -60.45, pntEle = Just 91.0, pntTime = Nothing}))
  , (SASA, ICAOData (Point {pntLat = -24.85, pntLon = -65.48333333333333, pntEle = Just 1238.0, pntTime = Nothing}))
  , (SASJ, ICAOData (Point {pntLat = -24.383333333333333, pntLon = -65.08333333333333, pntEle = Just 921.0, pntTime = Nothing}))
  , (SASO, ICAOData (Point {pntLat = -23.15, pntLon = -64.31666666666666, pntEle = Just 357.0, pntTime = Nothing}))
  , (SASQ, ICAOData (Point {pntLat = -22.1, pntLon = -65.6, pntEle = Just 3462.0, pntTime = Nothing}))
  , (SASR, ICAOData (Point {pntLat = -24.166666666666668, pntLon = -62.9, pntEle = Just 205.0, pntTime = Nothing}))
  , (SAST, ICAOData (Point {pntLat = -22.65, pntLon = -63.81666666666667, pntEle = Just 450.0, pntTime = Nothing}))
  , (SATK, ICAOData (Point {pntLat = -24.7, pntLon = -60.583333333333336, pntEle = Just 130.0, pntTime = Nothing}))
  , (SATU, ICAOData (Point {pntLat = -29.783333333333335, pntLon = -57.983333333333334, pntEle = Just 80.0, pntTime = Nothing}))
  , (SAVB, ICAOData (Point {pntLat = -41.96666666666667, pntLon = -71.51666666666667, pntEle = Just 337.0, pntTime = Nothing}))
  , (SAVC, ICAOData (Point {pntLat = -45.78333333333333, pntLon = -67.5, pntEle = Just 58.0, pntTime = Nothing}))
  , (SAVE, ICAOData (Point {pntLat = -42.93333333333333, pntLon = -71.15, pntEle = Just 789.0, pntTime = Nothing}))
  , (SAVO, ICAOData (Point {pntLat = -40.78333333333333, pntLon = -65.1, pntEle = Just 20.0, pntTime = Nothing}))
  , (SAVP, ICAOData (Point {pntLat = -43.81666666666667, pntLon = -68.88333333333334, pntEle = Just 460.0, pntTime = Nothing}))
  , (SAVT, ICAOData (Point {pntLat = -43.2, pntLon = -65.26666666666667, pntEle = Just 39.0, pntTime = Nothing}))
  , (SAVV, ICAOData (Point {pntLat = -40.85, pntLon = -63.016666666666666, pntEle = Just 6.0, pntTime = Nothing}))
  , (SAWA, ICAOData (Point {pntLat = -50.333333333333336, pntLon = -72.3, pntEle = Just 223.0, pntTime = Nothing}))
  , (SAWD, ICAOData (Point {pntLat = -47.733333333333334, pntLon = -65.91666666666667, pntEle = Just 81.0, pntTime = Nothing}))
  , (SAWE, ICAOData (Point {pntLat = -53.8, pntLon = -67.75, pntEle = Just 13.0, pntTime = Nothing}))
  , (SAWG, ICAOData (Point {pntLat = -51.61666666666667, pntLon = -69.28333333333333, pntEle = Just 20.0, pntTime = Nothing}))
  , (SAWH, ICAOData (Point {pntLat = -54.8, pntLon = -68.31666666666666, pntEle = Just 16.0, pntTime = Nothing}))
  , (SAWJ, ICAOData (Point {pntLat = -49.31666666666667, pntLon = -67.75, pntEle = Just 58.0, pntTime = Nothing}))
  , (SAWP, ICAOData (Point {pntLat = -46.516666666666666, pntLon = -71.01666666666667, pntEle = Just 429.0, pntTime = Nothing}))
  , (SAWR, ICAOData (Point {pntLat = -48.78333333333333, pntLon = -70.16666666666667, pntEle = Just 358.0, pntTime = Nothing}))
  , (SAWU, ICAOData (Point {pntLat = -50.016666666666666, pntLon = -68.56666666666666, pntEle = Just 113.0, pntTime = Nothing}))
  , (SAZA, ICAOData (Point {pntLat = -36.75, pntLon = -59.833333333333336, pntEle = Just 132.0, pntTime = Nothing}))
  , (SAZB, ICAOData (Point {pntLat = -38.733333333333334, pntLon = -62.166666666666664, pntEle = Just 75.0, pntTime = Nothing}))
  , (SAZD, ICAOData (Point {pntLat = -36.35, pntLon = -57.733333333333334, pntEle = Just 10.0, pntTime = Nothing}))
  , (SAZE, ICAOData (Point {pntLat = -37.6, pntLon = -62.38333333333333, pntEle = Just 304.0, pntTime = Nothing}))
  , (SAZG, ICAOData (Point {pntLat = -35.7, pntLon = -63.75, pntEle = Just 139.0, pntTime = Nothing}))
  , (SAZH, ICAOData (Point {pntLat = -38.333333333333336, pntLon = -60.25, pntEle = Just 115.0, pntTime = Nothing}))
  , (SAZM, ICAOData (Point {pntLat = -37.93333333333333, pntLon = -57.583333333333336, pntEle = Just 18.0, pntTime = Nothing}))
  , (SAZN, ICAOData (Point {pntLat = -38.95, pntLon = -68.13333333333334, pntEle = Just 270.0, pntTime = Nothing}))
  , (SAZP, ICAOData (Point {pntLat = -35.86666666666667, pntLon = -61.9, pntEle = Just 87.0, pntTime = Nothing}))
  , (SAZQ, ICAOData (Point {pntLat = -39.016666666666666, pntLon = -64.08333333333333, pntEle = Just 79.0, pntTime = Nothing}))
  , (SAZR, ICAOData (Point {pntLat = -36.56666666666667, pntLon = -64.26666666666667, pntEle = Just 190.0, pntTime = Nothing}))
  , (SAZS, ICAOData (Point {pntLat = -41.15, pntLon = -71.16666666666667, pntEle = Just 845.0, pntTime = Nothing}))
  , (SAZT, ICAOData (Point {pntLat = -37.233333333333334, pntLon = -59.25, pntEle = Just 175.0, pntTime = Nothing}))
  , (SAZV, ICAOData (Point {pntLat = -37.233333333333334, pntLon = -57.03333333333333, pntEle = Just 7.0, pntTime = Nothing}))
  , (SAZY, ICAOData (Point {pntLat = -40.083333333333336, pntLon = -71.13333333333334, pntEle = Just 779.0, pntTime = Nothing}))
  , (SBAA, ICAOData (Point {pntLat = -8.25, pntLon = -49.28333333333333, pntEle = Just 157.0, pntTime = Nothing}))
  , (SBAF, ICAOData (Point {pntLat = -22.866666666666667, pntLon = -43.36666666666667, pntEle = Just 34.0, pntTime = Nothing}))
  , (SBAN, ICAOData (Point {pntLat = -16.233333333333334, pntLon = -48.96666666666667, pntEle = Just 1137.0, pntTime = Nothing}))
  , (SBAR, ICAOData (Point {pntLat = -10.983333333333333, pntLon = -37.06666666666667, pntEle = Just 8.0, pntTime = Nothing}))
  , (SBAT, ICAOData (Point {pntLat = -9.866666666666667, pntLon = -56.1, pntEle = Just 288.0, pntTime = Nothing}))
  , (SBBC, ICAOData (Point {pntLat = -4.383333333333334, pntLon = -70.03333333333333, pntEle = Just 65.0, pntTime = Nothing}))
  , (SBBE, ICAOData (Point {pntLat = -1.3833333333333333, pntLon = -48.483333333333334, pntEle = Just 16.0, pntTime = Nothing}))
  , (SBBG, ICAOData (Point {pntLat = -31.35, pntLon = -54.11666666666667, pntEle = Just 180.0, pntTime = Nothing}))
  , (SBBH, ICAOData (Point {pntLat = -19.85, pntLon = -43.95, pntEle = Just 785.0, pntTime = Nothing}))
  , (SBBI, ICAOData (Point {pntLat = -25.433333333333334, pntLon = -49.266666666666666, pntEle = Just 924.0, pntTime = Nothing}))
  , (SBBQ, ICAOData (Point {pntLat = -21.25, pntLon = -43.766666666666666, pntEle = Just 1171.0, pntTime = Nothing}))
  , (SBBR, ICAOData (Point {pntLat = -15.866666666666667, pntLon = -47.93333333333333, pntEle = Just 1061.0, pntTime = Nothing}))
  , (SBBU, ICAOData (Point {pntLat = -22.316666666666666, pntLon = -49.06666666666667, pntEle = Just 590.0, pntTime = Nothing}))
  , (SBBV, ICAOData (Point {pntLat = 2.8333333333333335, pntLon = -60.7, pntEle = Just 84.0, pntTime = Nothing}))
  , (SBBW, ICAOData (Point {pntLat = -15.866666666666667, pntLon = -52.38333333333333, pntEle = Just 350.0, pntTime = Nothing}))
  , (SBCF, ICAOData (Point {pntLat = -19.933333333333334, pntLon = -43.93333333333333, pntEle = Just 850.0, pntTime = Nothing}))
  , (SBCG, ICAOData (Point {pntLat = -20.466666666666665, pntLon = -54.666666666666664, pntEle = Just 567.0, pntTime = Nothing}))
  , (SBCI, ICAOData (Point {pntLat = -7.316666666666666, pntLon = -47.46666666666667, pntEle = Just 183.0, pntTime = Nothing}))
  , (SBCO, ICAOData (Point {pntLat = -30.016666666666666, pntLon = -51.21666666666667, pntEle = Just 47.0, pntTime = Nothing}))
  , (SBCP, ICAOData (Point {pntLat = -21.75, pntLon = -41.333333333333336, pntEle = Just 11.0, pntTime = Nothing}))
  , (SBCR, ICAOData (Point {pntLat = -19.083333333333332, pntLon = -57.5, pntEle = Just 130.0, pntTime = Nothing}))
  , (SBCT, ICAOData (Point {pntLat = -25.516666666666666, pntLon = -49.166666666666664, pntEle = Just 908.0, pntTime = Nothing}))
  , (SBCV, ICAOData (Point {pntLat = -17.65, pntLon = -39.25, pntEle = Just 11.0, pntTime = Nothing}))
  , (SBCY, ICAOData (Point {pntLat = -15.65, pntLon = -56.1, pntEle = Just 187.0, pntTime = Nothing}))
  , (SBCZ, ICAOData (Point {pntLat = -7.633333333333333, pntLon = -72.66666666666667, pntEle = Just 170.0, pntTime = Nothing}))
  , (SBDN, ICAOData (Point {pntLat = -22.116666666666667, pntLon = -51.38333333333333, pntEle = Just 436.0, pntTime = Nothing}))
  , (SBEG, ICAOData (Point {pntLat = -3.033333333333333, pntLon = -60.05, pntEle = Just 2.0, pntTime = Nothing}))
  , (SBEK, ICAOData (Point {pntLat = -6.266666666666667, pntLon = -57.733333333333334, pntEle = Just 98.0, pntTime = Nothing}))
  , (SBES, ICAOData (Point {pntLat = -22.816666666666666, pntLon = -42.1, pntEle = Just 10.0, pntTime = Nothing}))
  , (SBFI, ICAOData (Point {pntLat = -25.516666666666666, pntLon = -54.583333333333336, pntEle = Just 180.0, pntTime = Nothing}))
  , (SBFL, ICAOData (Point {pntLat = -27.666666666666668, pntLon = -48.55, pntEle = Just 5.0, pntTime = Nothing}))
  , (SBFN, ICAOData (Point {pntLat = -3.85, pntLon = -32.416666666666664, pntEle = Just 56.0, pntTime = Nothing}))
  , (SBFZ, ICAOData (Point {pntLat = -3.783333333333333, pntLon = -38.53333333333333, pntEle = Just 25.0, pntTime = Nothing}))
  , (SBGA, ICAOData (Point {pntLat = -16.05, pntLon = -48.05, pntEle = Just 700.0, pntTime = Nothing}))
  , (SBGL, ICAOData (Point {pntLat = -22.816666666666666, pntLon = -43.25, pntEle = Just 6.0, pntTime = Nothing}))
  , (SBGO, ICAOData (Point {pntLat = -16.633333333333333, pntLon = -49.21666666666667, pntEle = Just 747.0, pntTime = Nothing}))
  , (SBGW, ICAOData (Point {pntLat = -22.783333333333335, pntLon = -45.2, pntEle = Just 537.0, pntTime = Nothing}))
  , (SBHT, ICAOData (Point {pntLat = -3.2, pntLon = -52.2, pntEle = Just 74.0, pntTime = Nothing}))
  , (SBIH, ICAOData (Point {pntLat = -4.25, pntLon = -56.0, pntEle = Just 34.0, pntTime = Nothing}))
  , (SBIL, ICAOData (Point {pntLat = -14.816666666666666, pntLon = -39.03333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (SBIZ, ICAOData (Point {pntLat = -5.533333333333333, pntLon = -47.5, pntEle = Just 123.0, pntTime = Nothing}))
  , (SBJF, ICAOData (Point {pntLat = -21.766666666666666, pntLon = -43.35, pntEle = Just 939.0, pntTime = Nothing}))
  , (SBJP, ICAOData (Point {pntLat = -7.1, pntLon = -34.86666666666667, pntEle = Just 7.0, pntTime = Nothing}))
  , (SBJR, ICAOData (Point {pntLat = -22.983333333333334, pntLon = -43.36666666666667, pntEle = Just 3.0, pntTime = Nothing}))
  , (SBKG, ICAOData (Point {pntLat = -7.216666666666667, pntLon = -35.88333333333333, pntEle = Just 548.0, pntTime = Nothing}))
  , (SBKP, ICAOData (Point {pntLat = -23.0, pntLon = -47.13333333333333, pntEle = Just 661.0, pntTime = Nothing}))
  , (SBLO, ICAOData (Point {pntLat = -23.333333333333332, pntLon = -51.13333333333333, pntEle = Just 569.0, pntTime = Nothing}))
  , (SBLP, ICAOData (Point {pntLat = -13.266666666666667, pntLon = -43.416666666666664, pntEle = Just 440.0, pntTime = Nothing}))
  , (SBMA, ICAOData (Point {pntLat = -5.35, pntLon = -49.15, pntEle = Just 95.0, pntTime = Nothing}))
  , (SBMG, ICAOData (Point {pntLat = -23.416666666666668, pntLon = -51.95, pntEle = Just 542.0, pntTime = Nothing}))
  , (SBMK, ICAOData (Point {pntLat = -16.716666666666665, pntLon = -43.86666666666667, pntEle = Just 646.0, pntTime = Nothing}))
  , (SBMN, ICAOData (Point {pntLat = -3.15, pntLon = -59.983333333333334, pntEle = Just 84.0, pntTime = Nothing}))
  , (SBMO, ICAOData (Point {pntLat = -9.516666666666667, pntLon = -35.78333333333333, pntEle = Just 117.0, pntTime = Nothing}))
  , (SBMQ, ICAOData (Point {pntLat = 3.333333333333333e-2, pntLon = -50.05, pntEle = Just 15.0, pntTime = Nothing}))
  , (SBMS, ICAOData (Point {pntLat = -5.2, pntLon = -37.36666666666667, pntEle = Just 23.0, pntTime = Nothing}))
  , (SBMT, ICAOData (Point {pntLat = -23.516666666666666, pntLon = -46.63333333333333, pntEle = Just 722.0, pntTime = Nothing}))
  , (SBMY, ICAOData (Point {pntLat = -5.816666666666666, pntLon = -61.3, pntEle = Just 50.0, pntTime = Nothing}))
  , (SBNT, ICAOData (Point {pntLat = -5.916666666666667, pntLon = -35.25, pntEle = Just 52.0, pntTime = Nothing}))
  , (SBOI, ICAOData (Point {pntLat = 3.8333333333333335, pntLon = -51.833333333333336, pntEle = Just 39.0, pntTime = Nothing}))
  , (SBPA, ICAOData (Point {pntLat = -30.0, pntLon = -51.18333333333333, pntEle = Just 3.0, pntTime = Nothing}))
  , (SBPB, ICAOData (Point {pntLat = -2.9166666666666665, pntLon = -41.75, pntEle = Just 5.0, pntTime = Nothing}))
  , (SBPC, ICAOData (Point {pntLat = -21.85, pntLon = -46.56666666666667, pntEle = Just 1260.0, pntTime = Nothing}))
  , (SBPF, ICAOData (Point {pntLat = -28.25, pntLon = -52.4, pntEle = Just 684.0, pntTime = Nothing}))
  , (SBPG, ICAOData (Point {pntLat = -25.516666666666666, pntLon = -48.516666666666666, pntEle = Just 5.0, pntTime = Nothing}))
  , (SBPK, ICAOData (Point {pntLat = -31.866666666666667, pntLon = -52.35, pntEle = Just 13.0, pntTime = Nothing}))
  , (SBPL, ICAOData (Point {pntLat = -9.35, pntLon = -40.55, pntEle = Just 385.0, pntTime = Nothing}))
  , (SBPN, ICAOData (Point {pntLat = -10.7, pntLon = -48.4, pntEle = Just 261.0, pntTime = Nothing}))
  , (SBPP, ICAOData (Point {pntLat = -22.55, pntLon = -55.7, pntEle = Just 657.0, pntTime = Nothing}))
  , (SBPV, ICAOData (Point {pntLat = -8.766666666666667, pntLon = -63.916666666666664, pntEle = Just 102.0, pntTime = Nothing}))
  , (SBQV, ICAOData (Point {pntLat = -14.95, pntLon = -40.88333333333333, pntEle = Just 840.0, pntTime = Nothing}))
  , (SBRB, ICAOData (Point {pntLat = -10.0, pntLon = -67.8, pntEle = Just 142.0, pntTime = Nothing}))
  , (SBRF, ICAOData (Point {pntLat = -8.066666666666666, pntLon = -34.85, pntEle = Just 19.0, pntTime = Nothing}))
  , (SBRJ, ICAOData (Point {pntLat = -22.9, pntLon = -43.166666666666664, pntEle = Just 3.0, pntTime = Nothing}))
  , (SBRS, ICAOData (Point {pntLat = -22.483333333333334, pntLon = -44.46666666666667, pntEle = Just 440.0, pntTime = Nothing}))
  , (SBSA, ICAOData (Point {pntLat = -22.016666666666666, pntLon = -47.88333333333333, pntEle = Just 856.0, pntTime = Nothing}))
  , (SBSC, ICAOData (Point {pntLat = -22.933333333333334, pntLon = -43.71666666666667, pntEle = Just 3.0, pntTime = Nothing}))
  , (SBSL, ICAOData (Point {pntLat = -2.6, pntLon = -44.233333333333334, pntEle = Just 53.0, pntTime = Nothing}))
  , (SBSM, ICAOData (Point {pntLat = -29.716666666666665, pntLon = -53.7, pntEle = Just 85.0, pntTime = Nothing}))
  , (SBSN, ICAOData (Point {pntLat = -2.4333333333333336, pntLon = -54.71666666666667, pntEle = Just 72.0, pntTime = Nothing}))
  , (SBSP, ICAOData (Point {pntLat = -23.616666666666667, pntLon = -46.65, pntEle = Just 803.0, pntTime = Nothing}))
  , (SBST, ICAOData (Point {pntLat = -23.933333333333334, pntLon = -46.3, pntEle = Just 3.0, pntTime = Nothing}))
  , (SBSV, ICAOData (Point {pntLat = -12.9, pntLon = -38.333333333333336, pntEle = Just 6.0, pntTime = Nothing}))
  , (SBTE, ICAOData (Point {pntLat = -5.05, pntLon = -42.81666666666667, pntEle = Just 69.0, pntTime = Nothing}))
  , (SBTF, ICAOData (Point {pntLat = -3.3666666666666667, pntLon = -64.68333333333334, pntEle = Just 47.0, pntTime = Nothing}))
  , (SBTK, ICAOData (Point {pntLat = -8.166666666666666, pntLon = -70.76666666666667, pntEle = Just 190.0, pntTime = Nothing}))
  , (SBTT, ICAOData (Point {pntLat = -3.6666666666666665, pntLon = -69.66666666666667, pntEle = Just 85.0, pntTime = Nothing}))
  , (SBTU, ICAOData (Point {pntLat = -3.716666666666667, pntLon = -49.71666666666667, pntEle = Just 40.0, pntTime = Nothing}))
  , (SBUA, ICAOData (Point {pntLat = -0.13333333333333333, pntLon = -67.08333333333333, pntEle = Just 90.0, pntTime = Nothing}))
  , (SBUF, ICAOData (Point {pntLat = -9.4, pntLon = -38.21666666666667, pntEle = Just 253.0, pntTime = Nothing}))
  , (SBUG, ICAOData (Point {pntLat = -29.783333333333335, pntLon = -57.03333333333333, pntEle = Just 74.0, pntTime = Nothing}))
  , (SBUR, ICAOData (Point {pntLat = -19.783333333333335, pntLon = -47.96666666666667, pntEle = Just 807.0, pntTime = Nothing}))
  , (SBVH, ICAOData (Point {pntLat = -12.7, pntLon = -60.1, pntEle = Just 612.0, pntTime = Nothing}))
  , (SBVT, ICAOData (Point {pntLat = -20.266666666666666, pntLon = -40.28333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (SBXV, ICAOData (Point {pntLat = -14.7, pntLon = -52.35, pntEle = Just 315.0, pntTime = Nothing}))
  , (SBYA, ICAOData (Point {pntLat = 0.6166666666666667, pntLon = -69.2, pntEle = Just 120.0, pntTime = Nothing}))
  , (SBYS, ICAOData (Point {pntLat = -21.983333333333334, pntLon = -47.333333333333336, pntEle = Just 597.0, pntTime = Nothing}))
  , (SCAR, ICAOData (Point {pntLat = -18.333333333333332, pntLon = -70.33333333333333, pntEle = Just 58.0, pntTime = Nothing}))
  , (SCBA, ICAOData (Point {pntLat = -45.916666666666664, pntLon = -71.68333333333334, pntEle = Just 520.0, pntTime = Nothing}))
  , (SCCC, ICAOData (Point {pntLat = -46.55, pntLon = -71.7, pntEle = Just 325.0, pntTime = Nothing}))
  , (SCCH, ICAOData (Point {pntLat = -36.56666666666667, pntLon = -72.03333333333333, pntEle = Just 124.0, pntTime = Nothing}))
  , (SCCI, ICAOData (Point {pntLat = -53.0, pntLon = -70.85, pntEle = Just 37.0, pntTime = Nothing}))
  , (SCCY, ICAOData (Point {pntLat = -45.583333333333336, pntLon = -72.11666666666666, pntEle = Just 310.0, pntTime = Nothing}))
  , (SCDA, ICAOData (Point {pntLat = -20.533333333333335, pntLon = -70.18333333333334, pntEle = Just 52.0, pntTime = Nothing}))
  , (SCEL, ICAOData (Point {pntLat = -33.38333333333333, pntLon = -70.78333333333333, pntEle = Just 475.0, pntTime = Nothing}))
  , (SCER, ICAOData (Point {pntLat = -32.78333333333333, pntLon = -71.51666666666667, pntEle = Just 8.0, pntTime = Nothing}))
  , (SCFA, ICAOData (Point {pntLat = -23.433333333333334, pntLon = -70.43333333333334, pntEle = Just 135.0, pntTime = Nothing}))
  , (SCHA, ICAOData (Point {pntLat = -27.3, pntLon = -70.41666666666667, pntEle = Just 291.0, pntTime = Nothing}))
  , (SCHR, ICAOData (Point {pntLat = -47.233333333333334, pntLon = -72.55, pntEle = Just 167.0, pntTime = Nothing}))
  , (SCIC, ICAOData (Point {pntLat = -34.96666666666667, pntLon = -71.23333333333333, pntEle = Just 228.0, pntTime = Nothing}))
  , (SCIE, ICAOData (Point {pntLat = -36.766666666666666, pntLon = -73.05, pntEle = Just 12.0, pntTime = Nothing}))
  , (SCIP, ICAOData (Point {pntLat = -27.15, pntLon = -109.41666666666667, pntEle = Just 51.0, pntTime = Nothing}))
  , (SCLL, ICAOData (Point {pntLat = -28.6, pntLon = -70.76666666666667, pntEle = Just 538.0, pntTime = Nothing}))
  , (SCRA, ICAOData (Point {pntLat = -26.316666666666666, pntLon = -70.61666666666666, pntEle = Just 30.0, pntTime = Nothing}))
  , (SCSE, ICAOData (Point {pntLat = -29.9, pntLon = -71.2, pntEle = Just 142.0, pntTime = Nothing}))
  , (SCTC, ICAOData (Point {pntLat = -38.75, pntLon = -72.63333333333334, pntEle = Just 114.0, pntTime = Nothing}))
  , (SCTE, ICAOData (Point {pntLat = -41.416666666666664, pntLon = -73.08333333333333, pntEle = Just 85.0, pntTime = Nothing}))
  , (SCVD, ICAOData (Point {pntLat = -39.61666666666667, pntLon = -73.08333333333333, pntEle = Just 19.0, pntTime = Nothing}))
  , (SEAM, ICAOData (Point {pntLat = -1.2, pntLon = -78.56666666666666, pntEle = Just 2520.0, pntTime = Nothing}))
  , (SEBC, ICAOData (Point {pntLat = -0.5833333333333334, pntLon = -80.4, pntEle = Just 3.0, pntTime = Nothing}))
  , (SECU, ICAOData (Point {pntLat = -2.8833333333333333, pntLon = -78.98333333333333, pntEle = Just 2530.0, pntTime = Nothing}))
  , (SEES, ICAOData (Point {pntLat = 0.9666666666666667, pntLon = -79.63333333333334, pntEle = Just 7.0, pntTime = Nothing}))
  , (SEGU, ICAOData (Point {pntLat = -2.15, pntLon = -79.88333333333334, pntEle = Just 4.0, pntTime = Nothing}))
  , (SEIB, ICAOData (Point {pntLat = 0.35, pntLon = -78.13333333333334, pntEle = Just 2228.0, pntTime = Nothing}))
  , (SELO, ICAOData (Point {pntLat = -4.0, pntLon = -79.36666666666666, pntEle = Just 1239.0, pntTime = Nothing}))
  , (SELT, ICAOData (Point {pntLat = -0.9166666666666666, pntLon = -78.61666666666666, pntEle = Just 2785.0, pntTime = Nothing}))
  , (SEMA, ICAOData (Point {pntLat = -4.366666666666666, pntLon = -79.93333333333334, pntEle = Just 430.0, pntTime = Nothing}))
  , (SEMH, ICAOData (Point {pntLat = -3.25, pntLon = -79.96666666666667, pntEle = Just 4.0, pntTime = Nothing}))
  , (SEMT, ICAOData (Point {pntLat = -0.95, pntLon = -80.68333333333334, pntEle = Just 13.0, pntTime = Nothing}))
  , (SEPA, ICAOData (Point {pntLat = -1.5, pntLon = -78.05, pntEle = Just 1043.0, pntTime = Nothing}))
  , (SEQU, ICAOData (Point {pntLat = -0.15, pntLon = -78.48333333333333, pntEle = Just 2811.0, pntTime = Nothing}))
  , (SESA, ICAOData (Point {pntLat = -2.2, pntLon = -80.98333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (SEST, ICAOData (Point {pntLat = -0.9, pntLon = -89.6, pntEle = Just 6.0, pntTime = Nothing}))
  , (SETI, ICAOData (Point {pntLat = -0.7833333333333333, pntLon = -75.51666666666667, pntEle = Just 220.0, pntTime = Nothing}))
  , (SETU, ICAOData (Point {pntLat = 0.8166666666666667, pntLon = -77.7, pntEle = Just 2950.0, pntTime = Nothing}))
  , (SFAL, ICAOData (Point {pntLat = -51.68333333333333, pntLon = -57.766666666666666, pntEle = Just 23.0, pntTime = Nothing}))
  , (SGAS, ICAOData (Point {pntLat = -25.266666666666666, pntLon = -57.63333333333333, pntEle = Just 101.0, pntTime = Nothing}))
  , (SGCO, ICAOData (Point {pntLat = -23.416666666666668, pntLon = -57.3, pntEle = Just 74.0, pntTime = Nothing}))
  , (SGEN, ICAOData (Point {pntLat = -27.316666666666666, pntLon = -55.833333333333336, pntEle = Just 91.0, pntTime = Nothing}))
  , (SGME, ICAOData (Point {pntLat = -22.016666666666666, pntLon = -60.6, pntEle = Just 172.0, pntTime = Nothing}))
  , (SGNA, ICAOData (Point {pntLat = -20.716666666666665, pntLon = -61.916666666666664, pntEle = Just 318.0, pntTime = Nothing}))
  , (SKAR, ICAOData (Point {pntLat = 4.5, pntLon = -75.71666666666667, pntEle = Just 1204.0, pntTime = Nothing}))
  , (SKAS, ICAOData (Point {pntLat = 0.5, pntLon = -76.5, pntEle = Just 254.0, pntTime = Nothing}))
  , (SKBG, ICAOData (Point {pntLat = 7.1, pntLon = -73.2, pntEle = Just 1189.0, pntTime = Nothing}))
  , (SKBO, ICAOData (Point {pntLat = 4.716666666666667, pntLon = -74.15, pntEle = Just 2547.0, pntTime = Nothing}))
  , (SKBQ, ICAOData (Point {pntLat = 10.883333333333333, pntLon = -74.78333333333333, pntEle = Just 14.0, pntTime = Nothing}))
  , (SKBU, ICAOData (Point {pntLat = 3.85, pntLon = -76.96666666666667, pntEle = Just 14.0, pntTime = Nothing}))
  , (SKCC, ICAOData (Point {pntLat = 7.933333333333334, pntLon = -72.51666666666667, pntEle = Just 250.0, pntTime = Nothing}))
  , (SKCG, ICAOData (Point {pntLat = 10.45, pntLon = -75.51666666666667, pntEle = Just 1.0, pntTime = Nothing}))
  , (SKCL, ICAOData (Point {pntLat = 3.55, pntLon = -76.38333333333334, pntEle = Just 961.0, pntTime = Nothing}))
  , (SKEJ, ICAOData (Point {pntLat = 7.016666666666667, pntLon = -73.8, pntEle = Just 126.0, pntTime = Nothing}))
  , (SKIB, ICAOData (Point {pntLat = 4.433333333333334, pntLon = -75.15, pntEle = Just 928.0, pntTime = Nothing}))
  , (SKIP, ICAOData (Point {pntLat = 0.8166666666666667, pntLon = -77.63333333333334, pntEle = Just 2961.0, pntTime = Nothing}))
  , (SKLC, ICAOData (Point {pntLat = 7.816666666666666, pntLon = -76.7, pntEle = Just 20.0, pntTime = Nothing}))
  , (SKLT, ICAOData (Point {pntLat = -4.166666666666667, pntLon = -69.95, pntEle = Just 84.0, pntTime = Nothing}))
  , (SKMD, ICAOData (Point {pntLat = 6.216666666666667, pntLon = -75.6, pntEle = Just 1490.0, pntTime = Nothing}))
  , (SKMR, ICAOData (Point {pntLat = 8.816666666666666, pntLon = -75.85, pntEle = Just 20.0, pntTime = Nothing}))
  , (SKMU, ICAOData (Point {pntLat = 1.1333333333333333, pntLon = -70.05, pntEle = Just 207.0, pntTime = Nothing}))
  , (SKNV, ICAOData (Point {pntLat = 2.966666666666667, pntLon = -75.3, pntEle = Just 439.0, pntTime = Nothing}))
  , (SKPC, ICAOData (Point {pntLat = 6.166666666666667, pntLon = -67.5, pntEle = Just 55.0, pntTime = Nothing}))
  , (SKPE, ICAOData (Point {pntLat = 4.816666666666666, pntLon = -75.73333333333333, pntEle = Just 1342.0, pntTime = Nothing}))
  , (SKPP, ICAOData (Point {pntLat = 2.466666666666667, pntLon = -76.6, pntEle = Just 1730.0, pntTime = Nothing}))
  , (SKPS, ICAOData (Point {pntLat = 1.4166666666666667, pntLon = -77.26666666666667, pntEle = Just 1826.0, pntTime = Nothing}))
  , (SKPV, ICAOData (Point {pntLat = 13.366666666666667, pntLon = -81.35, pntEle = Just 6.0, pntTime = Nothing}))
  , (SKRG, ICAOData (Point {pntLat = 6.133333333333334, pntLon = -75.43333333333334, pntEle = Just 2140.0, pntTime = Nothing}))
  , (SKRH, ICAOData (Point {pntLat = 11.533333333333333, pntLon = -72.93333333333334, pntEle = Just 4.0, pntTime = Nothing}))
  , (SKSJ, ICAOData (Point {pntLat = 2.5666666666666664, pntLon = -72.63333333333334, pntEle = Just 155.0, pntTime = Nothing}))
  , (SKSM, ICAOData (Point {pntLat = 11.133333333333333, pntLon = -74.23333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (SKSP, ICAOData (Point {pntLat = 12.583333333333334, pntLon = -81.71666666666667, pntEle = Just 1.0, pntTime = Nothing}))
  , (SKUC, ICAOData (Point {pntLat = 7.066666666666666, pntLon = -70.73333333333333, pntEle = Just 128.0, pntTime = Nothing}))
  , (SKUI, ICAOData (Point {pntLat = 5.716666666666667, pntLon = -76.61666666666666, pntEle = Just 53.0, pntTime = Nothing}))
  , (SKVP, ICAOData (Point {pntLat = 10.433333333333334, pntLon = -73.25, pntEle = Just 138.0, pntTime = Nothing}))
  , (SKVV, ICAOData (Point {pntLat = 4.166666666666667, pntLon = -73.61666666666666, pntEle = Just 423.0, pntTime = Nothing}))
  , (SLAP, ICAOData (Point {pntLat = -14.733333333333333, pntLon = -68.5, pntEle = Just 1415.0, pntTime = Nothing}))
  , (SLAS, ICAOData (Point {pntLat = -15.716666666666667, pntLon = -63.1, pntEle = Just 247.0, pntTime = Nothing}))
  , (SLCA, ICAOData (Point {pntLat = -20.0, pntLon = -63.53333333333333, pntEle = Just 798.0, pntTime = Nothing}))
  , (SLCB, ICAOData (Point {pntLat = -17.416666666666668, pntLon = -66.18333333333334, pntEle = Just 2548.0, pntTime = Nothing}))
  , (SLCN, ICAOData (Point {pntLat = -17.583333333333332, pntLon = -69.6, pntEle = Just 4054.0, pntTime = Nothing}))
  , (SLCO, ICAOData (Point {pntLat = -11.033333333333333, pntLon = -68.78333333333333, pntEle = Just 235.0, pntTime = Nothing}))
  , (SLCP, ICAOData (Point {pntLat = -16.15, pntLon = -62.016666666666666, pntEle = Just 497.0, pntTime = Nothing}))
  , (SLET, ICAOData (Point {pntLat = -17.8, pntLon = -63.18333333333333, pntEle = Just 418.0, pntTime = Nothing}))
  , (SLGY, ICAOData (Point {pntLat = -10.816666666666666, pntLon = -65.35, pntEle = Just 130.0, pntTime = Nothing}))
  , (SLJE, ICAOData (Point {pntLat = -17.8, pntLon = -60.733333333333334, pntEle = Just 284.0, pntTime = Nothing}))
  , (SLJO, ICAOData (Point {pntLat = -13.066666666666666, pntLon = -64.81666666666666, pntEle = Just 140.0, pntTime = Nothing}))
  , (SLJV, ICAOData (Point {pntLat = -16.266666666666666, pntLon = -62.46666666666667, pntEle = Just 534.0, pntTime = Nothing}))
  , (SLLP, ICAOData (Point {pntLat = -16.516666666666666, pntLon = -68.18333333333334, pntEle = Just 4058.0, pntTime = Nothing}))
  , (SLMG, ICAOData (Point {pntLat = -13.333333333333334, pntLon = -64.11666666666666, pntEle = Just 140.0, pntTime = Nothing}))
  , (SLOR, ICAOData (Point {pntLat = -17.966666666666665, pntLon = -67.06666666666666, pntEle = Just 3072.0, pntTime = Nothing}))
  , (SLPO, ICAOData (Point {pntLat = -19.55, pntLon = -65.73333333333333, pntEle = Just 3935.0, pntTime = Nothing}))
  , (SLPS, ICAOData (Point {pntLat = -18.983333333333334, pntLon = -57.81666666666667, pntEle = Just 134.0, pntTime = Nothing}))
  , (SLRB, ICAOData (Point {pntLat = -18.316666666666666, pntLon = -59.766666666666666, pntEle = Just 276.0, pntTime = Nothing}))
  , (SLRI, ICAOData (Point {pntLat = -11.0, pntLon = -66.11666666666666, pntEle = Just 141.0, pntTime = Nothing}))
  , (SLRQ, ICAOData (Point {pntLat = -14.466666666666667, pntLon = -67.56666666666666, pntEle = Just 204.0, pntTime = Nothing}))
  , (SLRY, ICAOData (Point {pntLat = -14.316666666666666, pntLon = -67.38333333333334, pntEle = Just 140.0, pntTime = Nothing}))
  , (SLSA, ICAOData (Point {pntLat = -13.766666666666667, pntLon = -65.43333333333334, pntEle = Just 144.0, pntTime = Nothing}))
  , (SLSB, ICAOData (Point {pntLat = -14.866666666666667, pntLon = -66.86666666666666, pntEle = Just 194.0, pntTime = Nothing}))
  , (SLSI, ICAOData (Point {pntLat = -16.383333333333333, pntLon = -60.96666666666667, pntEle = Just 413.0, pntTime = Nothing}))
  , (SLSM, ICAOData (Point {pntLat = -14.916666666666666, pntLon = -65.6, pntEle = Just 160.0, pntTime = Nothing}))
  , (SLSU, ICAOData (Point {pntLat = -19.016666666666666, pntLon = -65.3, pntEle = Just 2904.0, pntTime = Nothing}))
  , (SLTJ, ICAOData (Point {pntLat = -21.55, pntLon = -64.7, pntEle = Just 1854.0, pntTime = Nothing}))
  , (SLTR, ICAOData (Point {pntLat = -14.816666666666666, pntLon = -64.91666666666667, pntEle = Just 155.0, pntTime = Nothing}))
  , (SLVM, ICAOData (Point {pntLat = -21.25, pntLon = -63.45, pntEle = Just 398.0, pntTime = Nothing}))
  , (SLVR, ICAOData (Point {pntLat = -17.633333333333333, pntLon = -63.13333333333333, pntEle = Just 373.0, pntTime = Nothing}))
  , (SLYA, ICAOData (Point {pntLat = -21.95, pntLon = -63.65, pntEle = Just 645.0, pntTime = Nothing}))
  , (SMZY, ICAOData (Point {pntLat = 5.45, pntLon = -55.2, pntEle = Just 15.0, pntTime = Nothing}))
  , (SOCA, ICAOData (Point {pntLat = 4.833333333333333, pntLon = -52.36666666666667, pntEle = Just 105.0, pntTime = Nothing}))
  , (SOOM, ICAOData (Point {pntLat = 5.5, pntLon = -54.03333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (SPAY, ICAOData (Point {pntLat = -10.733333333333333, pntLon = -73.78333333333333, pntEle = Just 450.0, pntTime = Nothing}))
  , (SPCL, ICAOData (Point {pntLat = -8.416666666666666, pntLon = -74.6, pntEle = Just 148.0, pntTime = Nothing}))
  , (SPEO, ICAOData (Point {pntLat = -9.166666666666666, pntLon = -78.51666666666667, pntEle = Just 20.0, pntTime = Nothing}))
  , (SPGM, ICAOData (Point {pntLat = -9.133333333333333, pntLon = -75.95, pntEle = Just 664.0, pntTime = Nothing}))
  , (SPHI, ICAOData (Point {pntLat = -6.783333333333333, pntLon = -79.83333333333333, pntEle = Just 29.0, pntTime = Nothing}))
  , (SPHO, ICAOData (Point {pntLat = -13.133333333333333, pntLon = -74.21666666666667, pntEle = Just 2740.0, pntTime = Nothing}))
  , (SPHY, ICAOData (Point {pntLat = -13.716666666666667, pntLon = -73.35, pntEle = Just 3444.0, pntTime = Nothing}))
  , (SPHZ, ICAOData (Point {pntLat = -9.35, pntLon = -77.6, pntEle = Just 2750.0, pntTime = Nothing}))
  , (SPIM, ICAOData (Point {pntLat = -12.0, pntLon = -77.11666666666666, pntEle = Just 12.0, pntTime = Nothing}))
  , (SPJA, ICAOData (Point {pntLat = -6.05, pntLon = -77.15, pntEle = Just 792.0, pntTime = Nothing}))
  , (SPJI, ICAOData (Point {pntLat = -7.216666666666667, pntLon = -76.71666666666667, pntEle = Just 350.0, pntTime = Nothing}))
  , (SPJL, ICAOData (Point {pntLat = -15.483333333333333, pntLon = -70.15, pntEle = Just 3826.0, pntTime = Nothing}))
  , (SPJN, ICAOData (Point {pntLat = -15.383333333333333, pntLon = -75.16666666666667, pntEle = Just 60.0, pntTime = Nothing}))
  , (SPJR, ICAOData (Point {pntLat = -7.133333333333334, pntLon = -78.46666666666667, pntEle = Just 2620.0, pntTime = Nothing}))
  , (SPME, ICAOData (Point {pntLat = -3.55, pntLon = -80.4, pntEle = Just 25.0, pntTime = Nothing}))
  , (SPMS, ICAOData (Point {pntLat = -5.9, pntLon = -76.08333333333333, pntEle = Just 179.0, pntTime = Nothing}))
  , (SPNC, ICAOData (Point {pntLat = -9.9, pntLon = -75.75, pntEle = Just 1859.0, pntTime = Nothing}))
  , (SPPY, ICAOData (Point {pntLat = -6.216666666666667, pntLon = -77.83333333333333, pntEle = Just 2540.0, pntTime = Nothing}))
  , (SPQT, ICAOData (Point {pntLat = -3.75, pntLon = -73.25, pntEle = Just 125.0, pntTime = Nothing}))
  , (SPQU, ICAOData (Point {pntLat = -16.316666666666666, pntLon = -71.55, pntEle = Just 2538.0, pntTime = Nothing}))
  , (SPRU, ICAOData (Point {pntLat = -8.1, pntLon = -79.03333333333333, pntEle = Just 26.0, pntTime = Nothing}))
  , (SPSO, ICAOData (Point {pntLat = -13.75, pntLon = -76.28333333333333, pntEle = Just 8.0, pntTime = Nothing}))
  , (SPST, ICAOData (Point {pntLat = -6.45, pntLon = -76.38333333333334, pntEle = Just 281.0, pntTime = Nothing}))
  , (SPTN, ICAOData (Point {pntLat = -18.066666666666666, pntLon = -70.3, pntEle = Just 468.0, pntTime = Nothing}))
  , (SPTU, ICAOData (Point {pntLat = -12.633333333333333, pntLon = -69.2, pntEle = Just 265.0, pntTime = Nothing}))
  , (SPUR, ICAOData (Point {pntLat = -5.183333333333334, pntLon = -80.6, pntEle = Just 49.0, pntTime = Nothing}))
  , (SPYL, ICAOData (Point {pntLat = -4.566666666666666, pntLon = -81.25, pntEle = Just 85.0, pntTime = Nothing}))
  , (SPZO, ICAOData (Point {pntLat = -13.55, pntLon = -71.98333333333333, pntEle = Just 3248.0, pntTime = Nothing}))
  , (SUAA, ICAOData (Point {pntLat = -34.78333333333333, pntLon = -56.25, pntEle = Just 53.0, pntTime = Nothing}))
  , (SUAG, ICAOData (Point {pntLat = -30.383333333333333, pntLon = -56.5, pntEle = Just 123.0, pntTime = Nothing}))
  , (SUCA, ICAOData (Point {pntLat = -34.45, pntLon = -57.833333333333336, pntEle = Just 23.0, pntTime = Nothing}))
  , (SUDU, ICAOData (Point {pntLat = -33.35, pntLon = -56.5, pntEle = Just 93.0, pntTime = Nothing}))
  , (SUME, ICAOData (Point {pntLat = -33.25, pntLon = -58.06666666666667, pntEle = Just 22.0, pntTime = Nothing}))
  , (SUMO, ICAOData (Point {pntLat = -32.36666666666667, pntLon = -54.21666666666667, pntEle = Just 100.0, pntTime = Nothing}))
  , (SUMU, ICAOData (Point {pntLat = -34.833333333333336, pntLon = -56.0, pntEle = Just 32.0, pntTime = Nothing}))
  , (SUPU, ICAOData (Point {pntLat = -32.333333333333336, pntLon = -58.03333333333333, pntEle = Just 61.0, pntTime = Nothing}))
  , (SURV, ICAOData (Point {pntLat = -30.883333333333333, pntLon = -55.53333333333333, pntEle = Just 205.0, pntTime = Nothing}))
  , (SUSO, ICAOData (Point {pntLat = -31.383333333333333, pntLon = -57.95, pntEle = Just 34.0, pntTime = Nothing}))
  , (SUTB, ICAOData (Point {pntLat = -31.7, pntLon = -55.983333333333334, pntEle = Just 134.0, pntTime = Nothing}))
  , (SUTR, ICAOData (Point {pntLat = -33.21666666666667, pntLon = -54.38333333333333, pntEle = Just 46.0, pntTime = Nothing}))
  , (SVAC, ICAOData (Point {pntLat = 9.55, pntLon = -69.23333333333333, pntEle = Just 226.0, pntTime = Nothing}))
  , (SVBC, ICAOData (Point {pntLat = 10.116666666666667, pntLon = -64.68333333333334, pntEle = Just 7.0, pntTime = Nothing}))
  , (SVBI, ICAOData (Point {pntLat = 8.616666666666667, pntLon = -70.21666666666667, pntEle = Just 203.0, pntTime = Nothing}))
  , (SVBM, ICAOData (Point {pntLat = 10.066666666666666, pntLon = -69.31666666666666, pntEle = Just 613.0, pntTime = Nothing}))
  , (SVBS, ICAOData (Point {pntLat = 10.25, pntLon = -67.65, pntEle = Just 436.0, pntTime = Nothing}))
  , (SVCB, ICAOData (Point {pntLat = 8.15, pntLon = -63.55, pntEle = Just 43.0, pntTime = Nothing}))
  , (SVCL, ICAOData (Point {pntLat = 8.933333333333334, pntLon = -67.41666666666667, pntEle = Just 100.0, pntTime = Nothing}))
  , (SVCR, ICAOData (Point {pntLat = 11.416666666666666, pntLon = -69.68333333333334, pntEle = Just 16.0, pntTime = Nothing}))
  , (SVCU, ICAOData (Point {pntLat = 10.45, pntLon = -64.18333333333334, pntEle = Just 2.0, pntTime = Nothing}))
  , (SVFM, ICAOData (Point {pntLat = 10.5, pntLon = -66.88333333333334, pntEle = Just 835.0, pntTime = Nothing}))
  , (SVGD, ICAOData (Point {pntLat = 7.233333333333333, pntLon = -70.8, pntEle = Just 130.0, pntTime = Nothing}))
  , (SVGI, ICAOData (Point {pntLat = 10.583333333333334, pntLon = -62.31666666666667, pntEle = Just 13.0, pntTime = Nothing}))
  , (SVGU, ICAOData (Point {pntLat = 9.016666666666667, pntLon = -69.73333333333333, pntEle = Just 163.0, pntTime = Nothing}))
  , (SVJM, ICAOData (Point {pntLat = 9.916666666666666, pntLon = -67.33333333333333, pntEle = Just 429.0, pntTime = Nothing}))
  , (SVLO, ICAOData (Point {pntLat = 11.8, pntLon = -66.18333333333334, pntEle = Just 3.0, pntTime = Nothing}))
  , (SVMC, ICAOData (Point {pntLat = 10.566666666666666, pntLon = -71.73333333333333, pntEle = Just 66.0, pntTime = Nothing}))
  , (SVMD, ICAOData (Point {pntLat = 8.6, pntLon = -71.18333333333334, pntEle = Just 1479.0, pntTime = Nothing}))
  , (SVMI, ICAOData (Point {pntLat = 10.6, pntLon = -66.98333333333333, pntEle = Just 43.0, pntTime = Nothing}))
  , (SVMN, ICAOData (Point {pntLat = 9.816666666666666, pntLon = -70.93333333333334, pntEle = Just 27.0, pntTime = Nothing}))
  , (SVMT, ICAOData (Point {pntLat = 9.75, pntLon = -63.18333333333333, pntEle = Just 65.0, pntTime = Nothing}))
  , (SVPA, ICAOData (Point {pntLat = 5.6, pntLon = -67.5, pntEle = Just 73.0, pntTime = Nothing}))
  , (SVPC, ICAOData (Point {pntLat = 10.5, pntLon = -68.0, pntEle = Just 2.0, pntTime = Nothing}))
  , (SVSA, ICAOData (Point {pntLat = 7.85, pntLon = -72.45, pntEle = Just 377.0, pntTime = Nothing}))
  , (SVSE, ICAOData (Point {pntLat = 4.6, pntLon = -61.11666666666667, pntEle = Just 907.0, pntTime = Nothing}))
  , (SVSO, ICAOData (Point {pntLat = 7.583333333333333, pntLon = -72.06666666666666, pntEle = Just 327.0, pntTime = Nothing}))
  , (SVSR, ICAOData (Point {pntLat = 7.9, pntLon = -67.41666666666667, pntEle = Just 47.0, pntTime = Nothing}))
  , (SVTM, ICAOData (Point {pntLat = 7.3, pntLon = -61.45, pntEle = Just 180.0, pntTime = Nothing}))
  , (SVTR, ICAOData (Point {pntLat = 9.016666666666667, pntLon = -62.61666666666667, pntEle = Just 30.0, pntTime = Nothing}))
  , (SVVA, ICAOData (Point {pntLat = 10.166666666666666, pntLon = -67.93333333333334, pntEle = Just 430.0, pntTime = Nothing}))
  , (SVVL, ICAOData (Point {pntLat = 9.35, pntLon = -70.61666666666666, pntEle = Just 582.0, pntTime = Nothing}))
  , (SVVP, ICAOData (Point {pntLat = 9.216666666666667, pntLon = -66.01666666666667, pntEle = Just 125.0, pntTime = Nothing}))
  , (SWBC, ICAOData (Point {pntLat = -0.9833333333333333, pntLon = -62.583333333333336, pntEle = Just 40.0, pntTime = Nothing}))
  , (SYGT, ICAOData (Point {pntLat = 6.8, pntLon = -58.15, pntEle = Just 1.0, pntTime = Nothing}))
  , (SYTM, ICAOData (Point {pntLat = 6.5, pntLon = -58.25, pntEle = Just 28.0, pntTime = Nothing}))
  , (TAPA, ICAOData (Point {pntLat = 17.116666666666667, pntLon = -61.78333333333333, pntEle = Just 8.0, pntTime = Nothing}))
  , (TBPB, ICAOData (Point {pntLat = 13.066666666666666, pntLon = -59.483333333333334, pntEle = Just 50.0, pntTime = Nothing}))
  , (TDCF, ICAOData (Point {pntLat = 15.533333333333333, pntLon = -61.4, pntEle = Just 5.0, pntTime = Nothing}))
  , (TDPD, ICAOData (Point {pntLat = 15.533333333333333, pntLon = -61.3, pntEle = Just 13.0, pntTime = Nothing}))
  , (TDPR, ICAOData (Point {pntLat = 15.3, pntLon = -61.4, pntEle = Just 72.0, pntTime = Nothing}))
  , (TFFF, ICAOData (Point {pntLat = 14.6, pntLon = -61.0, pntEle = Just 5.0, pntTime = Nothing}))
  , (TFFJ, ICAOData (Point {pntLat = 17.9, pntLon = -62.85, pntEle = Just 48.0, pntTime = Nothing}))
  , (TFFR, ICAOData (Point {pntLat = 16.266666666666666, pntLon = -61.516666666666666, pntEle = Just 11.0, pntTime = Nothing}))
  , (TGPY, ICAOData (Point {pntLat = 12.0, pntLon = -61.78333333333333, pntEle = Just 6.0, pntTime = Nothing}))
  , (TIST, ICAOData (Point {pntLat = 18.333333333333332, pntLon = -64.98333333333333, pntEle = Just 7.0, pntTime = Nothing}))
  , (TISX, ICAOData (Point {pntLat = 17.7, pntLon = -64.8, pntEle = Just 20.0, pntTime = Nothing}))
  , (TJBQ, ICAOData (Point {pntLat = 18.5, pntLon = -67.13333333333334, pntEle = Just 72.0, pntTime = Nothing}))
  , (TJNR, ICAOData (Point {pntLat = 18.25, pntLon = -65.63333333333334, pntEle = Just 12.0, pntTime = Nothing}))
  , (TKPK, ICAOData (Point {pntLat = 17.3, pntLon = -62.68333333333333, pntEle = Just 48.0, pntTime = Nothing}))
  , (TKPN, ICAOData (Point {pntLat = 17.2, pntLon = -62.583333333333336, pntEle = Just 17.0, pntTime = Nothing}))
  , (TLPC, ICAOData (Point {pntLat = 14.016666666666667, pntLon = -61.0, pntEle = Just 2.0, pntTime = Nothing}))
  , (TLPL, ICAOData (Point {pntLat = 13.75, pntLon = -60.95, pntEle = Just 3.0, pntTime = Nothing}))
  , (TNCA, ICAOData (Point {pntLat = 12.5, pntLon = -70.01666666666667, pntEle = Just 18.0, pntTime = Nothing}))
  , (TNCB, ICAOData (Point {pntLat = 12.15, pntLon = -68.28333333333333, pntEle = Just 6.0, pntTime = Nothing}))
  , (TNCC, ICAOData (Point {pntLat = 12.2, pntLon = -68.96666666666667, pntEle = Just 9.0, pntTime = Nothing}))
  , (TNCE, ICAOData (Point {pntLat = 17.483333333333334, pntLon = -62.983333333333334, pntEle = Just 38.0, pntTime = Nothing}))
  , (TNCM, ICAOData (Point {pntLat = 18.05, pntLon = -63.11666666666667, pntEle = Just 4.0, pntTime = Nothing}))
  , (TTPP, ICAOData (Point {pntLat = 10.616666666666667, pntLon = -61.35, pntEle = Just 12.0, pntTime = Nothing}))
  , (TTPT, ICAOData (Point {pntLat = 11.15, pntLon = -60.833333333333336, pntEle = Just 3.0, pntTime = Nothing}))
  , (TUPJ, ICAOData (Point {pntLat = 18.45, pntLon = -64.53333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (TVSV, ICAOData (Point {pntLat = 13.133333333333333, pntLon = -61.2, pntEle = Just 9.0, pntTime = Nothing}))
  , (TXKF, ICAOData (Point {pntLat = 32.36666666666667, pntLon = -64.68333333333334, pntEle = Just 6.0, pntTime = Nothing}))
  , (UAAA, ICAOData (Point {pntLat = 43.233333333333334, pntLon = 76.93333333333334, pntEle = Just 847.0, pntTime = Nothing}))
  , (UAII, ICAOData (Point {pntLat = 42.31666666666667, pntLon = 69.7, pntEle = Just 552.0, pntTime = Nothing}))
  , (UAKK, ICAOData (Point {pntLat = 49.8, pntLon = 73.15, pntEle = Just 553.0, pntTime = Nothing}))
  , (UAOO, ICAOData (Point {pntLat = 44.85, pntLon = 65.5, pntEle = Just 129.0, pntTime = Nothing}))
  , (UARR, ICAOData (Point {pntLat = 51.25, pntLon = 51.28333333333333, pntEle = Just 36.0, pntTime = Nothing}))
  , (UATA, ICAOData (Point {pntLat = 46.78333333333333, pntLon = 61.65, pntEle = Just 56.0, pntTime = Nothing}))
  , (UATT, ICAOData (Point {pntLat = 50.28333333333333, pntLon = 57.15, pntEle = Just 227.0, pntTime = Nothing}))
  , (UBBB, ICAOData (Point {pntLat = 40.45, pntLon = 50.06666666666667, pntEle = Just (-6.0), pntTime = Nothing}))
  , (UEEE, ICAOData (Point {pntLat = 62.016666666666666, pntLon = 129.71666666666667, pntEle = Just 103.0, pntTime = Nothing}))
  , (UELL, ICAOData (Point {pntLat = 56.833333333333336, pntLon = 124.86666666666666, pntEle = Just 859.0, pntTime = Nothing}))
  , (UGEE, ICAOData (Point {pntLat = 40.13333333333333, pntLon = 44.46666666666667, pntEle = Just 890.0, pntTime = Nothing}))
  , (UGGG, ICAOData (Point {pntLat = 41.68333333333333, pntLon = 44.95, pntEle = Just 467.0, pntTime = Nothing}))
  , (UGMM, ICAOData (Point {pntLat = 41.93333333333333, pntLon = 44.583333333333336, pntEle = Just 551.0, pntTime = Nothing}))
  , (UHBP, ICAOData (Point {pntLat = 53.06666666666667, pntLon = 132.98333333333332, pntEle = Just 543.0, pntTime = Nothing}))
  , (UHHH, ICAOData (Point {pntLat = 48.516666666666666, pntLon = 135.16666666666666, pntEle = Just 72.0, pntTime = Nothing}))
  , (UHHO, ICAOData (Point {pntLat = 49.45, pntLon = 136.56666666666666, pntEle = Just 29.0, pntTime = Nothing}))
  , (UHMA, ICAOData (Point {pntLat = 64.78333333333333, pntLon = 177.56666666666666, pntEle = Just 62.0, pntTime = Nothing}))
  , (UHMD, ICAOData (Point {pntLat = 64.41666666666667, pntLon = -173.23333333333332, pntEle = Just 3.0, pntTime = Nothing}))
  , (UHMM, ICAOData (Point {pntLat = 59.55, pntLon = 150.78333333333333, pntEle = Just 118.0, pntTime = Nothing}))
  , (UHPP, ICAOData (Point {pntLat = 53.083333333333336, pntLon = 158.58333333333334, pntEle = Just 24.0, pntTime = Nothing}))
  , (UHSS, ICAOData (Point {pntLat = 46.95, pntLon = 142.71666666666667, pntEle = Just 31.0, pntTime = Nothing}))
  , (UHWW, ICAOData (Point {pntLat = 43.11666666666667, pntLon = 131.93333333333334, pntEle = Just 184.0, pntTime = Nothing}))
  , (UIAA, ICAOData (Point {pntLat = 52.083333333333336, pntLon = 113.48333333333333, pntEle = Just 685.0, pntTime = Nothing}))
  , (UIII, ICAOData (Point {pntLat = 52.266666666666666, pntLon = 104.31666666666666, pntEle = Just 513.0, pntTime = Nothing}))
  , (UIIO, ICAOData (Point {pntLat = 52.81666666666667, pntLon = 104.76666666666667, pntEle = Just 525.0, pntTime = Nothing}))
  , (UIKB, ICAOData (Point {pntLat = 57.85, pntLon = 114.23333333333333, pntEle = Just 278.0, pntTime = Nothing}))
  , (UIKK, ICAOData (Point {pntLat = 57.766666666666666, pntLon = 108.06666666666666, pntEle = Just 258.0, pntTime = Nothing}))
  , (UINN, ICAOData (Point {pntLat = 54.88333333333333, pntLon = 99.03333333333333, pntEle = Just 410.0, pntTime = Nothing}))
  , (UIUH, ICAOData (Point {pntLat = 52.166666666666664, pntLon = 109.78333333333333, pntEle = Just 666.0, pntTime = Nothing}))
  , (UIUU, ICAOData (Point {pntLat = 51.833333333333336, pntLon = 107.6, pntEle = Just 510.0, pntTime = Nothing}))
  , (UKBB, ICAOData (Point {pntLat = 50.333333333333336, pntLon = 30.966666666666665, pntEle = Just 121.0, pntTime = Nothing}))
  , (UKFF, ICAOData (Point {pntLat = 44.68333333333333, pntLon = 34.13333333333333, pntEle = Just 180.0, pntTime = Nothing}))
  , (UKHH, ICAOData (Point {pntLat = 49.96666666666667, pntLon = 36.13333333333333, pntEle = Just 154.0, pntTime = Nothing}))
  , (UKII, ICAOData (Point {pntLat = 47.016666666666666, pntLon = 28.983333333333334, pntEle = Just 180.0, pntTime = Nothing}))
  , (UKKK, ICAOData (Point {pntLat = 50.4, pntLon = 30.566666666666666, pntEle = Just 166.0, pntTime = Nothing}))
  , (UKLL, ICAOData (Point {pntLat = 49.81666666666667, pntLon = 23.95, pntEle = Just 319.0, pntTime = Nothing}))
  , (UKLR, ICAOData (Point {pntLat = 50.583333333333336, pntLon = 26.133333333333333, pntEle = Just 231.0, pntTime = Nothing}))
  , (UKOO, ICAOData (Point {pntLat = 46.43333333333333, pntLon = 30.766666666666666, pntEle = Just 42.0, pntTime = Nothing}))
  , (ULAA, ICAOData (Point {pntLat = 64.5, pntLon = 40.733333333333334, pntEle = Just 13.0, pntTime = Nothing}))
  , (ULAK, ICAOData (Point {pntLat = 61.233333333333334, pntLon = 46.71666666666667, pntEle = Just 56.0, pntTime = Nothing}))
  , (ULLI, ICAOData (Point {pntLat = 59.96666666666667, pntLon = 30.3, pntEle = Just 4.0, pntTime = Nothing}))
  , (ULMM, ICAOData (Point {pntLat = 68.96666666666667, pntLon = 33.05, pntEle = Just 51.0, pntTime = Nothing}))
  , (ULOL, ICAOData (Point {pntLat = 56.35, pntLon = 30.616666666666667, pntEle = Just 98.0, pntTime = Nothing}))
  , (ULWT, ICAOData (Point {pntLat = 59.88333333333333, pntLon = 42.75, pntEle = Just 134.0, pntTime = Nothing}))
  , (ULWW, ICAOData (Point {pntLat = 59.31666666666667, pntLon = 39.916666666666664, pntEle = Just 131.0, pntTime = Nothing}))
  , (UMII, ICAOData (Point {pntLat = 55.166666666666664, pntLon = 30.216666666666665, pntEle = Just 176.0, pntTime = Nothing}))
  , (UMMS, ICAOData (Point {pntLat = 53.93333333333333, pntLon = 27.633333333333333, pntEle = Just 234.0, pntTime = Nothing}))
  , (UMRR, ICAOData (Point {pntLat = 56.96666666666667, pntLon = 24.05, pntEle = Just 7.0, pntTime = Nothing}))
  , (UMRW, ICAOData (Point {pntLat = 57.4, pntLon = 21.533333333333335, pntEle = Just 3.0, pntTime = Nothing}))
  , (UNBB, ICAOData (Point {pntLat = 53.43333333333333, pntLon = 83.51666666666667, pntEle = Just 252.0, pntTime = Nothing}))
  , (UNII, ICAOData (Point {pntLat = 58.45, pntLon = 92.15, pntEle = Just 78.0, pntTime = Nothing}))
  , (UNNN, ICAOData (Point {pntLat = 55.083333333333336, pntLon = 82.9, pntEle = Just 177.0, pntTime = Nothing}))
  , (URKK, ICAOData (Point {pntLat = 45.03333333333333, pntLon = 39.15, pntEle = Just 33.0, pntTime = Nothing}))
  , (URMM, ICAOData (Point {pntLat = 44.233333333333334, pntLon = 43.06666666666667, pntEle = Just 314.0, pntTime = Nothing}))
  , (URRR, ICAOData (Point {pntLat = 47.266666666666666, pntLon = 39.81666666666667, pntEle = Just 77.0, pntTime = Nothing}))
  , (URSS, ICAOData (Point {pntLat = 43.43333333333333, pntLon = 39.9, pntEle = Just 16.0, pntTime = Nothing}))
  , (URWI, ICAOData (Point {pntLat = 46.36666666666667, pntLon = 44.333333333333336, pntEle = Just 150.0, pntTime = Nothing}))
  , (URWW, ICAOData (Point {pntLat = 48.78333333333333, pntLon = 44.36666666666667, pntEle = Just 145.0, pntTime = Nothing}))
  , (USHH, ICAOData (Point {pntLat = 61.016666666666666, pntLon = 69.03333333333333, pntEle = Just 40.0, pntTime = Nothing}))
  , (USRR, ICAOData (Point {pntLat = 61.25, pntLon = 73.5, pntEle = Just 44.0, pntTime = Nothing}))
  , (USSS, ICAOData (Point {pntLat = 56.833333333333336, pntLon = 60.63333333333333, pntEle = Just 237.0, pntTime = Nothing}))
  , (UTDD, ICAOData (Point {pntLat = 38.55, pntLon = 68.78333333333333, pntEle = Just 803.0, pntTime = Nothing}))
  , (UTED, ICAOData (Point {pntLat = 40.11666666666667, pntLon = 67.83333333333333, pntEle = Just 345.0, pntTime = Nothing}))
  , (UTSM, ICAOData (Point {pntLat = 41.733333333333334, pntLon = 64.61666666666666, pntEle = Just 238.0, pntTime = Nothing}))
  , (UTSS, ICAOData (Point {pntLat = 39.56666666666667, pntLon = 66.95, pntEle = Just 724.0, pntTime = Nothing}))
  , (UTST, ICAOData (Point {pntLat = 37.233333333333334, pntLon = 67.26666666666667, pntEle = Just 302.0, pntTime = Nothing}))
  , (UTTT, ICAOData (Point {pntLat = 41.266666666666666, pntLon = 69.26666666666667, pntEle = Just 489.0, pntTime = Nothing}))
  , (UUBP, ICAOData (Point {pntLat = 53.25, pntLon = 34.31666666666667, pntEle = Just 217.0, pntTime = Nothing}))
  , (UUEM, ICAOData (Point {pntLat = 56.9, pntLon = 35.88333333333333, pntEle = Just 137.0, pntTime = Nothing}))
  , (UUOO, ICAOData (Point {pntLat = 51.65, pntLon = 39.25, pntEle = Just 154.0, pntTime = Nothing}))
  , (UUYT, ICAOData (Point {pntLat = 61.68333333333333, pntLon = 53.68333333333333, pntEle = Just 139.0, pntTime = Nothing}))
  , (UUYY, ICAOData (Point {pntLat = 61.71666666666667, pntLon = 50.833333333333336, pntEle = Just 119.0, pntTime = Nothing}))
  , (UWPP, ICAOData (Point {pntLat = 53.11666666666667, pntLon = 45.016666666666666, pntEle = Just 174.0, pntTime = Nothing}))
  , (UWWW, ICAOData (Point {pntLat = 53.2, pntLon = 50.15, pntEle = Just 44.0, pntTime = Nothing}))
  , (VAAH, ICAOData (Point {pntLat = 23.066666666666666, pntLon = 72.63333333333334, pntEle = Just 55.0, pntTime = Nothing}))
  , (VAAK, ICAOData (Point {pntLat = 20.7, pntLon = 77.06666666666666, pntEle = Just 305.0, pntTime = Nothing}))
  , (VAAU, ICAOData (Point {pntLat = 19.85, pntLon = 75.4, pntEle = Just 582.0, pntTime = Nothing}))
  , (VABB, ICAOData (Point {pntLat = 19.116666666666667, pntLon = 72.85, pntEle = Just 8.0, pntTime = Nothing}))
  , (VABI, ICAOData (Point {pntLat = 31.25, pntLon = 76.66666666666667, pntEle = Just 589.0, pntTime = Nothing}))
  , (VABJ, ICAOData (Point {pntLat = 23.25, pntLon = 69.66666666666667, pntEle = Just 78.0, pntTime = Nothing}))
  , (VABM, ICAOData (Point {pntLat = 15.85, pntLon = 74.61666666666666, pntEle = Just 758.0, pntTime = Nothing}))
  , (VABP, ICAOData (Point {pntLat = 23.283333333333335, pntLon = 77.35, pntEle = Just 523.0, pntTime = Nothing}))
  , (VABV, ICAOData (Point {pntLat = 21.75, pntLon = 72.2, pntEle = Just 5.0, pntTime = Nothing}))
  , (VAGO, ICAOData (Point {pntLat = 15.383333333333333, pntLon = 73.81666666666666, pntEle = Just 42.0, pntTime = Nothing}))
  , (VAID, ICAOData (Point {pntLat = 22.716666666666665, pntLon = 75.8, pntEle = Just 561.0, pntTime = Nothing}))
  , (VAJB, ICAOData (Point {pntLat = 23.2, pntLon = 79.95, pntEle = Just 391.0, pntTime = Nothing}))
  , (VAKD, ICAOData (Point {pntLat = 21.833333333333332, pntLon = 76.36666666666666, pntEle = Just 317.0, pntTime = Nothing}))
  , (VAKP, ICAOData (Point {pntLat = 16.7, pntLon = 74.23333333333333, pntEle = Just 569.0, pntTime = Nothing}))
  , (VANP, ICAOData (Point {pntLat = 21.1, pntLon = 79.05, pntEle = Just 308.0, pntTime = Nothing}))
  , (VARK, ICAOData (Point {pntLat = 22.3, pntLon = 70.78333333333333, pntEle = Just 134.0, pntTime = Nothing}))
  , (VASL, ICAOData (Point {pntLat = 17.666666666666668, pntLon = 75.9, pntEle = Just 477.0, pntTime = Nothing}))
  , (VBBM, ICAOData (Point {pntLat = 24.266666666666666, pntLon = 97.2, pntEle = Just 111.0, pntTime = Nothing}))
  , (VBBS, ICAOData (Point {pntLat = 16.766666666666666, pntLon = 94.76666666666667, pntEle = Just 9.0, pntTime = Nothing}))
  , (VBCI, ICAOData (Point {pntLat = 14.116666666666667, pntLon = 93.36666666666666, pntEle = Just 2.0, pntTime = Nothing}))
  , (VBHL, ICAOData (Point {pntLat = 24.866666666666667, pntLon = 94.91666666666667, pntEle = Just 130.0, pntTime = Nothing}))
  , (VBKG, ICAOData (Point {pntLat = 21.3, pntLon = 99.61666666666666, pntEle = Just 827.0, pntTime = Nothing}))
  , (VBKP, ICAOData (Point {pntLat = 19.416666666666668, pntLon = 93.55, pntEle = Just 5.0, pntTime = Nothing}))
  , (VBLS, ICAOData (Point {pntLat = 22.933333333333334, pntLon = 97.75, pntEle = Just 747.0, pntTime = Nothing}))
  , (VBMK, ICAOData (Point {pntLat = 25.366666666666667, pntLon = 97.4, pntEle = Just 145.0, pntTime = Nothing}))
  , (VBML, ICAOData (Point {pntLat = 20.833333333333332, pntLon = 95.83333333333333, pntEle = Just 214.0, pntTime = Nothing}))
  , (VBMM, ICAOData (Point {pntLat = 16.5, pntLon = 97.61666666666666, pntEle = Just 21.0, pntTime = Nothing}))
  , (VBPA, ICAOData (Point {pntLat = 16.75, pntLon = 97.66666666666667, pntEle = Just 9.0, pntTime = Nothing}))
  , (VBPR, ICAOData (Point {pntLat = 18.8, pntLon = 95.21666666666667, pntEle = Just 58.0, pntTime = Nothing}))
  , (VBPT, ICAOData (Point {pntLat = 27.333333333333332, pntLon = 97.41666666666667, pntEle = Just 409.0, pntTime = Nothing}))
  , (VBRM, ICAOData (Point {pntLat = 21.983333333333334, pntLon = 96.1, pntEle = Just 74.0, pntTime = Nothing}))
  , (VBRN, ICAOData (Point {pntLat = 12.433333333333334, pntLon = 98.6, pntEle = Just 36.0, pntTime = Nothing}))
  , (VBRR, ICAOData (Point {pntLat = 16.9, pntLon = 96.18333333333334, pntEle = Just 28.0, pntTime = Nothing}))
  , (VBSY, ICAOData (Point {pntLat = 18.466666666666665, pntLon = 94.35, pntEle = Just 9.0, pntTime = Nothing}))
  , (VBTV, ICAOData (Point {pntLat = 14.1, pntLon = 98.21666666666667, pntEle = Just 16.0, pntTime = Nothing}))
  , (VCBI, ICAOData (Point {pntLat = 7.166666666666667, pntLon = 79.88333333333334, pntEle = Just 8.0, pntTime = Nothing}))
  , (VCCA, ICAOData (Point {pntLat = 8.333333333333334, pntLon = 80.41666666666667, pntEle = Just 89.0, pntTime = Nothing}))
  , (VCCB, ICAOData (Point {pntLat = 7.716666666666667, pntLon = 81.7, pntEle = Just 5.0, pntTime = Nothing}))
  , (VCCC, ICAOData (Point {pntLat = 6.816666666666666, pntLon = 79.88333333333334, pntEle = Just 5.0, pntTime = Nothing}))
  , (VCCT, ICAOData (Point {pntLat = 8.583333333333334, pntLon = 81.25, pntEle = Just 3.0, pntTime = Nothing}))
  , (VDKC, ICAOData (Point {pntLat = 12.0, pntLon = 105.45, pntEle = Just 16.0, pntTime = Nothing}))
  , (VDPP, ICAOData (Point {pntLat = 11.55, pntLon = 104.85, pntEle = Just 10.0, pntTime = Nothing}))
  , (VDSR, ICAOData (Point {pntLat = 13.366666666666667, pntLon = 103.85, pntEle = Just 15.0, pntTime = Nothing}))
  , (VEAT, ICAOData (Point {pntLat = 23.883333333333333, pntLon = 91.25, pntEle = Just 15.0, pntTime = Nothing}))
  , (VEBD, ICAOData (Point {pntLat = 26.633333333333333, pntLon = 88.31666666666666, pntEle = Just 126.0, pntTime = Nothing}))
  , (VEBS, ICAOData (Point {pntLat = 20.25, pntLon = 85.83333333333333, pntEle = Just 45.0, pntTime = Nothing}))
  , (VECC, ICAOData (Point {pntLat = 22.65, pntLon = 88.45, pntEle = Just 4.0, pntTime = Nothing}))
  , (VECX, ICAOData (Point {pntLat = 9.15, pntLon = 92.81666666666666, pntEle = Just 10.0, pntTime = Nothing}))
  , (VEGK, ICAOData (Point {pntLat = 26.75, pntLon = 83.36666666666666, pntEle = Just 76.0, pntTime = Nothing}))
  , (VEGT, ICAOData (Point {pntLat = 26.1, pntLon = 91.58333333333333, pntEle = Just 47.0, pntTime = Nothing}))
  , (VEGY, ICAOData (Point {pntLat = 24.75, pntLon = 84.95, pntEle = Just 110.0, pntTime = Nothing}))
  , (VEIM, ICAOData (Point {pntLat = 24.766666666666666, pntLon = 93.9, pntEle = Just 780.0, pntTime = Nothing}))
  , (VEJH, ICAOData (Point {pntLat = 21.916666666666668, pntLon = 84.08333333333333, pntEle = Just 228.0, pntTime = Nothing}))
  , (VEJS, ICAOData (Point {pntLat = 22.816666666666666, pntLon = 86.18333333333334, pntEle = Just 140.0, pntTime = Nothing}))
  , (VELR, ICAOData (Point {pntLat = 27.233333333333334, pntLon = 94.11666666666666, pntEle = Just 101.0, pntTime = Nothing}))
  , (VEMN, ICAOData (Point {pntLat = 27.483333333333334, pntLon = 95.01666666666667, pntEle = Just 110.0, pntTime = Nothing}))
  , (VEPB, ICAOData (Point {pntLat = 11.666666666666666, pntLon = 92.71666666666667, pntEle = Just 73.0, pntTime = Nothing}))
  , (VEPT, ICAOData (Point {pntLat = 25.6, pntLon = 85.1, pntEle = Just 51.0, pntTime = Nothing}))
  , (VERC, ICAOData (Point {pntLat = 23.316666666666666, pntLon = 85.31666666666666, pntEle = Just 652.0, pntTime = Nothing}))
  , (VGCB, ICAOData (Point {pntLat = 21.433333333333334, pntLon = 91.96666666666667, pntEle = Just 2.0, pntTime = Nothing}))
  , (VGEG, ICAOData (Point {pntLat = 22.266666666666666, pntLon = 91.81666666666666, pntEle = Just 4.0, pntTime = Nothing}))
  , (VGIS, ICAOData (Point {pntLat = 24.133333333333333, pntLon = 89.05, pntEle = Just 13.0, pntTime = Nothing}))
  , (VGJR, ICAOData (Point {pntLat = 23.183333333333334, pntLon = 89.16666666666667, pntEle = Just 6.0, pntTime = Nothing}))
  , (VGRJ, ICAOData (Point {pntLat = 24.366666666666667, pntLon = 88.7, pntEle = Just 17.0, pntTime = Nothing}))
  , (VGSY, ICAOData (Point {pntLat = 24.9, pntLon = 91.88333333333334, pntEle = Just 34.0, pntTime = Nothing}))
  , (VGTJ, ICAOData (Point {pntLat = 23.766666666666666, pntLon = 90.38333333333334, pntEle = Just 8.0, pntTime = Nothing}))
  , (VGZR, ICAOData (Point {pntLat = 23.85, pntLon = 90.4, pntEle = Just 9.0, pntTime = Nothing}))
  , (VHCH, ICAOData (Point {pntLat = 22.2, pntLon = 114.01666666666667, pntEle = Just 79.0, pntTime = Nothing}))
  , (VHHH, ICAOData (Point {pntLat = 22.333333333333332, pntLon = 114.18333333333334, pntEle = Just 5.0, pntTime = Nothing}))
  , (VIAG, ICAOData (Point {pntLat = 27.15, pntLon = 77.96666666666667, pntEle = Just 168.0, pntTime = Nothing}))
  , (VIAL, ICAOData (Point {pntLat = 25.45, pntLon = 81.73333333333333, pntEle = Just 97.0, pntTime = Nothing}))
  , (VIAR, ICAOData (Point {pntLat = 31.633333333333333, pntLon = 74.86666666666666, pntEle = Just 229.0, pntTime = Nothing}))
  , (VIBN, ICAOData (Point {pntLat = 25.45, pntLon = 82.86666666666666, pntEle = Just 81.0, pntTime = Nothing}))
  , (VIBY, ICAOData (Point {pntLat = 28.366666666666667, pntLon = 79.4, pntEle = Just 167.0, pntTime = Nothing}))
  , (VICX, ICAOData (Point {pntLat = 26.4, pntLon = 80.4, pntEle = Just 123.0, pntTime = Nothing}))
  , (VIDD, ICAOData (Point {pntLat = 28.583333333333332, pntLon = 77.2, pntEle = Just 211.0, pntTime = Nothing}))
  , (VIDP, ICAOData (Point {pntLat = 28.566666666666666, pntLon = 77.11666666666666, pntEle = Just 220.0, pntTime = Nothing}))
  , (VIGR, ICAOData (Point {pntLat = 26.233333333333334, pntLon = 78.25, pntEle = Just 205.0, pntTime = Nothing}))
  , (VIHR, ICAOData (Point {pntLat = 29.166666666666668, pntLon = 75.73333333333333, pntEle = Just 216.0, pntTime = Nothing}))
  , (VIJN, ICAOData (Point {pntLat = 25.45, pntLon = 78.58333333333333, pntEle = Just 250.0, pntTime = Nothing}))
  , (VIJO, ICAOData (Point {pntLat = 26.3, pntLon = 73.01666666666667, pntEle = Just 217.0, pntTime = Nothing}))
  , (VIJP, ICAOData (Point {pntLat = 26.816666666666666, pntLon = 75.8, pntEle = Just 385.0, pntTime = Nothing}))
  , (VIKO, ICAOData (Point {pntLat = 25.15, pntLon = 75.85, pntEle = Just 273.0, pntTime = Nothing}))
  , (VILK, ICAOData (Point {pntLat = 26.75, pntLon = 80.88333333333334, pntEle = Just 122.0, pntTime = Nothing}))
  , (VIST, ICAOData (Point {pntLat = 24.566666666666666, pntLon = 80.83333333333333, pntEle = Just 315.0, pntTime = Nothing}))
  , (VIUD, ICAOData (Point {pntLat = 24.616666666666667, pntLon = 73.88333333333334, pntEle = Just 509.0, pntTime = Nothing}))
  , (VLAP, ICAOData (Point {pntLat = 14.8, pntLon = 106.83333333333333, pntEle = Just 105.0, pntTime = Nothing}))
  , (VLIP, ICAOData (Point {pntLat = 15.116666666666667, pntLon = 105.16666666666667, pntEle = Just 102.0, pntTime = Nothing}))
  , (VLLB, ICAOData (Point {pntLat = 19.883333333333333, pntLon = 102.13333333333334, pntEle = Just 305.0, pntTime = Nothing}))
  , (VLSB, ICAOData (Point {pntLat = 19.233333333333334, pntLon = 101.73333333333333, pntEle = Just 326.0, pntTime = Nothing}))
  , (VLSK, ICAOData (Point {pntLat = 16.55, pntLon = 104.65, pntEle = Just 155.0, pntTime = Nothing}))
  , (VLSV, ICAOData (Point {pntLat = 15.683333333333334, pntLon = 106.43333333333334, pntEle = Just 168.0, pntTime = Nothing}))
  , (VLTK, ICAOData (Point {pntLat = 17.383333333333333, pntLon = 104.65, pntEle = Just 52.0, pntTime = Nothing}))
  , (VLVT, ICAOData (Point {pntLat = 17.95, pntLon = 102.56666666666666, pntEle = Just 171.0, pntTime = Nothing}))
  , (VMMC, ICAOData (Point {pntLat = 22.166666666666668, pntLon = 113.56666666666666, pntEle = Just 114.0, pntTime = Nothing}))
  , (VNBW, ICAOData (Point {pntLat = 27.516666666666666, pntLon = 83.45, pntEle = Just 109.0, pntTime = Nothing}))
  , (VNJL, ICAOData (Point {pntLat = 29.283333333333335, pntLon = 82.16666666666667, pntEle = Just 2300.0, pntTime = Nothing}))
  , (VNKT, ICAOData (Point {pntLat = 27.7, pntLon = 85.36666666666666, pntEle = Just 1337.0, pntTime = Nothing}))
  , (VNPK, ICAOData (Point {pntLat = 28.216666666666665, pntLon = 84.0, pntEle = Just 827.0, pntTime = Nothing}))
  , (VNSI, ICAOData (Point {pntLat = 27.166666666666668, pntLon = 84.98333333333333, pntEle = Just 137.0, pntTime = Nothing}))
  , (VNSK, ICAOData (Point {pntLat = 28.6, pntLon = 81.61666666666666, pntEle = Just 720.0, pntTime = Nothing}))
  , (VNTJ, ICAOData (Point {pntLat = 27.35, pntLon = 87.66666666666667, pntEle = Just 1732.0, pntTime = Nothing}))
  , (VNVT, ICAOData (Point {pntLat = 26.483333333333334, pntLon = 87.26666666666667, pntEle = Just 72.0, pntTime = Nothing}))
  , (VOBI, ICAOData (Point {pntLat = 15.15, pntLon = 76.85, pntEle = Just 448.0, pntTime = Nothing}))
  , (VOBZ, ICAOData (Point {pntLat = 16.533333333333335, pntLon = 80.8, pntEle = Just 21.0, pntTime = Nothing}))
  , (VOCB, ICAOData (Point {pntLat = 11.033333333333333, pntLon = 77.05, pntEle = Just 396.0, pntTime = Nothing}))
  , (VOCC, ICAOData (Point {pntLat = 9.95, pntLon = 76.26666666666667, pntEle = Just 1.0, pntTime = Nothing}))
  , (VOCP, ICAOData (Point {pntLat = 14.483333333333333, pntLon = 78.83333333333333, pntEle = Just 129.0, pntTime = Nothing}))
  , (VOHY, ICAOData (Point {pntLat = 17.45, pntLon = 78.46666666666667, pntEle = Just 530.0, pntTime = Nothing}))
  , (VOMD, ICAOData (Point {pntLat = 9.833333333333334, pntLon = 78.08333333333333, pntEle = Just 139.0, pntTime = Nothing}))
  , (VOML, ICAOData (Point {pntLat = 12.916666666666666, pntLon = 74.88333333333334, pntEle = Just 102.0, pntTime = Nothing}))
  , (VOMM, ICAOData (Point {pntLat = 13.0, pntLon = 80.18333333333334, pntEle = Just 10.0, pntTime = Nothing}))
  , (VOTR, ICAOData (Point {pntLat = 10.766666666666667, pntLon = 78.71666666666667, pntEle = Just 85.0, pntTime = Nothing}))
  , (VOTV, ICAOData (Point {pntLat = 8.466666666666667, pntLon = 76.95, pntEle = Just 4.0, pntTime = Nothing}))
  , (VOVR, ICAOData (Point {pntLat = 12.916666666666666, pntLon = 79.15, pntEle = Just 213.0, pntTime = Nothing}))
  , (VRGN, ICAOData (Point {pntLat = -0.6833333333333333, pntLon = 73.15, pntEle = Just 2.0, pntTime = Nothing}))
  , (VRMM, ICAOData (Point {pntLat = 4.2, pntLon = 73.53333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (VTBC, ICAOData (Point {pntLat = 12.6, pntLon = 102.11666666666666, pntEle = Just 3.0, pntTime = Nothing}))
  , (VTBD, ICAOData (Point {pntLat = 13.916666666666666, pntLon = 100.6, pntEle = Just 4.0, pntTime = Nothing}))
  , (VTBG, ICAOData (Point {pntLat = 14.016666666666667, pntLon = 99.53333333333333, pntEle = Just 28.0, pntTime = Nothing}))
  , (VTBI, ICAOData (Point {pntLat = 14.05, pntLon = 101.36666666666666, pntEle = Just 5.0, pntTime = Nothing}))
  , (VTBJ, ICAOData (Point {pntLat = 13.15, pntLon = 100.06666666666666, pntEle = Just 2.0, pntTime = Nothing}))
  , (VTBP, ICAOData (Point {pntLat = 11.833333333333334, pntLon = 99.83333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (VTBS, ICAOData (Point {pntLat = 13.366666666666667, pntLon = 100.98333333333333, pntEle = Just 1.0, pntTime = Nothing}))
  , (VTBU, ICAOData (Point {pntLat = 12.633333333333333, pntLon = 101.35, pntEle = Just 3.0, pntTime = Nothing}))
  , (VTCC, ICAOData (Point {pntLat = 18.783333333333335, pntLon = 98.98333333333333, pntEle = Just 312.0, pntTime = Nothing}))
  , (VTCH, ICAOData (Point {pntLat = 19.3, pntLon = 97.83333333333333, pntEle = Just 267.0, pntTime = Nothing}))
  , (VTCL, ICAOData (Point {pntLat = 18.283333333333335, pntLon = 99.51666666666667, pntEle = Just 241.0, pntTime = Nothing}))
  , (VTCN, ICAOData (Point {pntLat = 18.766666666666666, pntLon = 100.76666666666667, pntEle = Just 200.0, pntTime = Nothing}))
  , (VTCP, ICAOData (Point {pntLat = 18.166666666666668, pntLon = 100.16666666666667, pntEle = Just 161.0, pntTime = Nothing}))
  , (VTCR, ICAOData (Point {pntLat = 19.916666666666668, pntLon = 99.83333333333333, pntEle = Just 394.0, pntTime = Nothing}))
  , (VTCS, ICAOData (Point {pntLat = 18.166666666666668, pntLon = 97.93333333333334, pntEle = Just 212.0, pntTime = Nothing}))
  , (VTPH, ICAOData (Point {pntLat = 12.583333333333334, pntLon = 99.95, pntEle = Just 5.0, pntTime = Nothing}))
  , (VTPM, ICAOData (Point {pntLat = 16.666666666666668, pntLon = 98.55, pntEle = Just 196.0, pntTime = Nothing}))
  , (VTPN, ICAOData (Point {pntLat = 15.8, pntLon = 100.16666666666667, pntEle = Just 34.0, pntTime = Nothing}))
  , (VTPS, ICAOData (Point {pntLat = 16.783333333333335, pntLon = 100.26666666666667, pntEle = Just 44.0, pntTime = Nothing}))
  , (VTPT, ICAOData (Point {pntLat = 16.883333333333333, pntLon = 99.15, pntEle = Just 121.0, pntTime = Nothing}))
  , (VTPU, ICAOData (Point {pntLat = 17.616666666666667, pntLon = 100.1, pntEle = Just 63.0, pntTime = Nothing}))
  , (VTSA, ICAOData (Point {pntLat = 6.65, pntLon = 100.08333333333333, pntEle = Just 4.0, pntTime = Nothing}))
  , (VTSB, ICAOData (Point {pntLat = 9.116666666666667, pntLon = 99.35, pntEle = Just 10.0, pntTime = Nothing}))
  , (VTSD, ICAOData (Point {pntLat = 10.483333333333333, pntLon = 99.18333333333334, pntEle = Just 3.0, pntTime = Nothing}))
  , (VTSH, ICAOData (Point {pntLat = 7.2, pntLon = 100.6, pntEle = Just 4.0, pntTime = Nothing}))
  , (VTSK, ICAOData (Point {pntLat = 6.783333333333333, pntLon = 101.15, pntEle = Just 5.0, pntTime = Nothing}))
  , (VTSN, ICAOData (Point {pntLat = 8.466666666666667, pntLon = 99.96666666666667, pntEle = Just 7.0, pntTime = Nothing}))
  , (VTSP, ICAOData (Point {pntLat = 8.116666666666667, pntLon = 98.31666666666666, pntEle = Just 6.0, pntTime = Nothing}))
  , (VTSR, ICAOData (Point {pntLat = 9.983333333333333, pntLon = 98.61666666666666, pntEle = Just 7.0, pntTime = Nothing}))
  , (VTSS, ICAOData (Point {pntLat = 6.916666666666667, pntLon = 100.43333333333334, pntEle = Just 27.0, pntTime = Nothing}))
  , (VTST, ICAOData (Point {pntLat = 7.516666666666667, pntLon = 99.61666666666666, pntEle = Just 14.0, pntTime = Nothing}))
  , (VTUB, ICAOData (Point {pntLat = 16.533333333333335, pntLon = 104.71666666666667, pntEle = Just 138.0, pntTime = Nothing}))
  , (VTUC, ICAOData (Point {pntLat = 15.8, pntLon = 102.03333333333333, pntEle = Just 182.0, pntTime = Nothing}))
  , (VTUD, ICAOData (Point {pntLat = 17.383333333333333, pntLon = 102.8, pntEle = Just 177.0, pntTime = Nothing}))
  , (VTUK, ICAOData (Point {pntLat = 16.433333333333334, pntLon = 102.83333333333333, pntEle = Just 165.0, pntTime = Nothing}))
  , (VTUL, ICAOData (Point {pntLat = 17.45, pntLon = 101.73333333333333, pntEle = Just 253.0, pntTime = Nothing}))
  , (VTUM, ICAOData (Point {pntLat = 17.866666666666667, pntLon = 102.71666666666667, pntEle = Just 174.0, pntTime = Nothing}))
  , (VTUN, ICAOData (Point {pntLat = 14.966666666666667, pntLon = 102.08333333333333, pntEle = Just 187.0, pntTime = Nothing}))
  , (VTUP, ICAOData (Point {pntLat = 17.416666666666668, pntLon = 104.78333333333333, pntEle = Just 146.0, pntTime = Nothing}))
  , (VTUR, ICAOData (Point {pntLat = 16.05, pntLon = 103.68333333333334, pntEle = Just 140.0, pntTime = Nothing}))
  , (VTUS, ICAOData (Point {pntLat = 17.15, pntLon = 104.13333333333334, pntEle = Just 171.0, pntTime = Nothing}))
  , (VTUU, ICAOData (Point {pntLat = 15.25, pntLon = 104.86666666666666, pntEle = Just 123.0, pntTime = Nothing}))
  , (VVDN, ICAOData (Point {pntLat = 16.033333333333335, pntLon = 108.18333333333334, pntEle = Just 7.0, pntTime = Nothing}))
  , (VVNB, ICAOData (Point {pntLat = 21.016666666666666, pntLon = 105.8, pntEle = Just 6.0, pntTime = Nothing}))
  , (VVNT, ICAOData (Point {pntLat = 12.25, pntLon = 109.2, pntEle = Just 10.0, pntTime = Nothing}))
  , (VVPB, ICAOData (Point {pntLat = 16.4, pntLon = 107.68333333333334, pntEle = Just 17.0, pntTime = Nothing}))
  , (VVPK, ICAOData (Point {pntLat = 13.983333333333333, pntLon = 108.0, pntEle = Just 801.0, pntTime = Nothing}))
  , (VVQN, ICAOData (Point {pntLat = 13.766666666666667, pntLon = 109.21666666666667, pntEle = Just 6.0, pntTime = Nothing}))
  , (VVTS, ICAOData (Point {pntLat = 10.816666666666666, pntLon = 106.66666666666667, pntEle = Just 19.0, pntTime = Nothing}))
  , (VVVH, ICAOData (Point {pntLat = 18.7, pntLon = 105.66666666666667, pntEle = Just 6.0, pntTime = Nothing}))
  , (VYSW, ICAOData (Point {pntLat = 20.133333333333333, pntLon = 92.88333333333334, pntEle = Just 4.0, pntTime = Nothing}))
  , (VYYY, ICAOData (Point {pntLat = 16.766666666666666, pntLon = 96.16666666666667, pntEle = Just 14.0, pntTime = Nothing}))
  , (WAAA, ICAOData (Point {pntLat = -5.066666666666666, pntLon = 119.55, pntEle = Just 14.0, pntTime = Nothing}))
  , (WAAB, ICAOData (Point {pntLat = -5.466666666666667, pntLon = 122.61666666666666, pntEle = Just 2.0, pntTime = Nothing}))
  , (WAAU, ICAOData (Point {pntLat = -4.1, pntLon = 122.43333333333334, pntEle = Just 50.0, pntTime = Nothing}))
  , (WABB, ICAOData (Point {pntLat = -1.1833333333333333, pntLon = 136.11666666666667, pntEle = Just 11.0, pntTime = Nothing}))
  , (WABI, ICAOData (Point {pntLat = -3.3333333333333335, pntLon = 135.5, pntEle = Just 3.0, pntTime = Nothing}))
  , (WABN, ICAOData (Point {pntLat = -4.716666666666667, pntLon = 136.43333333333334, pntEle = Just 3.0, pntTime = Nothing}))
  , (WABO, ICAOData (Point {pntLat = -1.8666666666666667, pntLon = 136.23333333333332, pntEle = Just 3.0, pntTime = Nothing}))
  , (WABT, ICAOData (Point {pntLat = -3.9166666666666665, pntLon = 136.36666666666667, pntEle = Just 1770.0, pntTime = Nothing}))
  , (WAJI, ICAOData (Point {pntLat = -1.8333333333333335, pntLon = 138.71666666666667, pntEle = Just 3.0, pntTime = Nothing}))
  , (WAJJ, ICAOData (Point {pntLat = -2.5666666666666664, pntLon = 140.48333333333332, pntEle = Just 99.0, pntTime = Nothing}))
  , (WAJW, ICAOData (Point {pntLat = -4.066666666666666, pntLon = 138.95, pntEle = Just 1660.0, pntTime = Nothing}))
  , (WAKK, ICAOData (Point {pntLat = -8.466666666666667, pntLon = 140.38333333333333, pntEle = Just 3.0, pntTime = Nothing}))
  , (WAKT, ICAOData (Point {pntLat = -6.1, pntLon = 140.3, pntEle = Just 16.0, pntTime = Nothing}))
  , (WAMA, ICAOData (Point {pntLat = 1.8166666666666667, pntLon = 127.83333333333333, pntEle = Just 8.0, pntTime = Nothing}))
  , (WAMG, ICAOData (Point {pntLat = 0.5166666666666667, pntLon = 123.06666666666666, pntEle = Just 2.0, pntTime = Nothing}))
  , (WAMH, ICAOData (Point {pntLat = 3.5833333333333335, pntLon = 125.46666666666667, pntEle = Just 38.0, pntTime = Nothing}))
  , (WAMI, ICAOData (Point {pntLat = 1.0166666666666666, pntLon = 120.8, pntEle = Just 2.0, pntTime = Nothing}))
  , (WAML, ICAOData (Point {pntLat = -0.6833333333333333, pntLon = 119.73333333333333, pntEle = Just 6.0, pntTime = Nothing}))
  , (WAMM, ICAOData (Point {pntLat = 1.5333333333333332, pntLon = 124.91666666666667, pntEle = Just 80.0, pntTime = Nothing}))
  , (WAMP, ICAOData (Point {pntLat = -1.3833333333333333, pntLon = 120.73333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (WAMT, ICAOData (Point {pntLat = 0.7833333333333333, pntLon = 127.38333333333334, pntEle = Just 23.0, pntTime = Nothing}))
  , (WAMW, ICAOData (Point {pntLat = -0.9, pntLon = 122.78333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (WAPA, ICAOData (Point {pntLat = -3.35, pntLon = 128.88333333333333, pntEle = Just 10.0, pntTime = Nothing}))
  , (WAPH, ICAOData (Point {pntLat = -1.6166666666666667, pntLon = 124.55, pntEle = Just 3.0, pntTime = Nothing}))
  , (WAPI, ICAOData (Point {pntLat = -7.983333333333333, pntLon = 131.3, pntEle = Just 24.0, pntTime = Nothing}))
  , (WAPN, ICAOData (Point {pntLat = -2.0833333333333335, pntLon = 126.0, pntEle = Just 2.0, pntTime = Nothing}))
  , (WAPP, ICAOData (Point {pntLat = -3.7, pntLon = 128.08333333333334, pntEle = Just 12.0, pntTime = Nothing}))
  , (WAPR, ICAOData (Point {pntLat = -3.25, pntLon = 127.08333333333333, pntEle = Just 20.0, pntTime = Nothing}))
  , (WASF, ICAOData (Point {pntLat = -2.8833333333333333, pntLon = 132.25, pntEle = Just 130.0, pntTime = Nothing}))
  , (WASK, ICAOData (Point {pntLat = -3.6666666666666665, pntLon = 133.75, pntEle = Just 3.0, pntTime = Nothing}))
  , (WASR, ICAOData (Point {pntLat = -0.8833333333333333, pntLon = 134.05, pntEle = Just 3.0, pntTime = Nothing}))
  , (WASS, ICAOData (Point {pntLat = -0.9333333333333333, pntLon = 131.11666666666667, pntEle = Just 3.0, pntTime = Nothing}))
  , (WBGB, ICAOData (Point {pntLat = 3.2, pntLon = 113.03333333333333, pntEle = Just 2.0, pntTime = Nothing}))
  , (WBGG, ICAOData (Point {pntLat = 1.4833333333333334, pntLon = 110.33333333333333, pntEle = Just 27.0, pntTime = Nothing}))
  , (WBGR, ICAOData (Point {pntLat = 4.333333333333333, pntLon = 113.98333333333333, pntEle = Just 17.0, pntTime = Nothing}))
  , (WBGS, ICAOData (Point {pntLat = 2.3333333333333335, pntLon = 111.83333333333333, pntEle = Just 8.0, pntTime = Nothing}))
  , (WBKK, ICAOData (Point {pntLat = 5.933333333333334, pntLon = 116.05, pntEle = Just 3.0, pntTime = Nothing}))
  , (WBKL, ICAOData (Point {pntLat = 5.3, pntLon = 115.25, pntEle = Just 30.0, pntTime = Nothing}))
  , (WBKS, ICAOData (Point {pntLat = 5.9, pntLon = 118.06666666666666, pntEle = Just 12.0, pntTime = Nothing}))
  , (WBKT, ICAOData (Point {pntLat = 6.916666666666667, pntLon = 116.83333333333333, pntEle = Just 3.0, pntTime = Nothing}))
  , (WBKW, ICAOData (Point {pntLat = 4.266666666666667, pntLon = 117.88333333333334, pntEle = Just 18.0, pntTime = Nothing}))
  , (WBSB, ICAOData (Point {pntLat = 4.933333333333334, pntLon = 114.93333333333334, pntEle = Just 22.0, pntTime = Nothing}))
  , (WIAA, ICAOData (Point {pntLat = 5.866666666666667, pntLon = 95.31666666666666, pntEle = Just 126.0, pntTime = Nothing}))
  , (WIAG, ICAOData (Point {pntLat = -4.45, pntLon = 105.18333333333334, pntEle = Just 19.0, pntTime = Nothing}))
  , (WIAM, ICAOData (Point {pntLat = -7.333333333333333, pntLon = 108.25, pntEle = Just 335.0, pntTime = Nothing}))
  , (WIAR, ICAOData (Point {pntLat = -7.616666666666667, pntLon = 111.51666666666667, pntEle = Just 110.0, pntTime = Nothing}))
  , (WIAS, ICAOData (Point {pntLat = -7.966666666666667, pntLon = 112.7, pntEle = Just 526.0, pntTime = Nothing}))
  , (WIBB, ICAOData (Point {pntLat = 0.4666666666666667, pntLon = 101.45, pntEle = Just 31.0, pntTime = Nothing}))
  , (WIIA, ICAOData (Point {pntLat = -6.233333333333333, pntLon = 106.65, pntEle = Just 46.0, pntTime = Nothing}))
  , (WIIB, ICAOData (Point {pntLat = -6.9, pntLon = 107.58333333333333, pntEle = Just 740.0, pntTime = Nothing}))
  , (WIIH, ICAOData (Point {pntLat = -6.25, pntLon = 106.9, pntEle = Just 30.0, pntTime = Nothing}))
  , (WIII, ICAOData (Point {pntLat = -6.116666666666666, pntLon = 106.65, pntEle = Just 8.0, pntTime = Nothing}))
  , (WIIJ, ICAOData (Point {pntLat = -7.783333333333333, pntLon = 110.43333333333334, pntEle = Just 107.0, pntTime = Nothing}))
  , (WIIK, ICAOData (Point {pntLat = -6.55, pntLon = 107.66666666666667, pntEle = Just 110.0, pntTime = Nothing}))
  , (WIIL, ICAOData (Point {pntLat = -7.733333333333333, pntLon = 109.01666666666667, pntEle = Just 6.0, pntTime = Nothing}))
  , (WIIS, ICAOData (Point {pntLat = -6.983333333333333, pntLon = 110.38333333333334, pntEle = Just 3.0, pntTime = Nothing}))
  , (WIIT, ICAOData (Point {pntLat = -5.266666666666667, pntLon = 105.18333333333334, pntEle = Just 96.0, pntTime = Nothing}))
  , (WIKB, ICAOData (Point {pntLat = 1.1166666666666667, pntLon = 104.11666666666666, pntEle = Just 24.0, pntTime = Nothing}))
  , (WIKD, ICAOData (Point {pntLat = -2.75, pntLon = 107.75, pntEle = Just 44.0, pntTime = Nothing}))
  , (WIKK, ICAOData (Point {pntLat = -2.1666666666666665, pntLon = 106.13333333333334, pntEle = Just 33.0, pntTime = Nothing}))
  , (WIKN, ICAOData (Point {pntLat = 0.9166666666666666, pntLon = 104.53333333333333, pntEle = Just 18.0, pntTime = Nothing}))
  , (WIKS, ICAOData (Point {pntLat = -0.48333333333333334, pntLon = 104.58333333333333, pntEle = Just 31.0, pntTime = Nothing}))
  , (WIMB, ICAOData (Point {pntLat = 1.5, pntLon = 97.63333333333334, pntEle = Just 6.0, pntTime = Nothing}))
  , (WIMG, ICAOData (Point {pntLat = -0.8833333333333333, pntLon = 100.35, pntEle = Just 3.0, pntTime = Nothing}))
  , (WIMM, ICAOData (Point {pntLat = 3.5666666666666664, pntLon = 98.68333333333334, pntEle = Just 25.0, pntTime = Nothing}))
  , (WIMS, ICAOData (Point {pntLat = 1.55, pntLon = 98.88333333333334, pntEle = Just 3.0, pntTime = Nothing}))
  , (WIOI, ICAOData (Point {pntLat = 1.0833333333333333, pntLon = 109.66666666666667, pntEle = Just 38.0, pntTime = Nothing}))
  , (WIOK, ICAOData (Point {pntLat = -1.85, pntLon = 109.96666666666667, pntEle = Just 9.0, pntTime = Nothing}))
  , (WION, ICAOData (Point {pntLat = 3.95, pntLon = 108.38333333333334, pntEle = Just 2.0, pntTime = Nothing}))
  , (WIOO, ICAOData (Point {pntLat = -0.15, pntLon = 109.4, pntEle = Just 3.0, pntTime = Nothing}))
  , (WIOS, ICAOData (Point {pntLat = 0.11666666666666667, pntLon = 111.53333333333333, pntEle = Just 30.0, pntTime = Nothing}))
  , (WIPA, ICAOData (Point {pntLat = -1.6333333333333333, pntLon = 103.65, pntEle = Just 25.0, pntTime = Nothing}))
  , (WIPH, ICAOData (Point {pntLat = -2.7666666666666666, pntLon = 101.36666666666666, pntEle = Just 782.0, pntTime = Nothing}))
  , (WIPL, ICAOData (Point {pntLat = -3.8833333333333333, pntLon = 102.33333333333333, pntEle = Just 16.0, pntTime = Nothing}))
  , (WIPP, ICAOData (Point {pntLat = -2.9, pntLon = 104.7, pntEle = Just 10.0, pntTime = Nothing}))
  , (WIPR, ICAOData (Point {pntLat = 0.4666666666666667, pntLon = 102.31666666666666, pntEle = Just 46.0, pntTime = Nothing}))
  , (WITC, ICAOData (Point {pntLat = 4.25, pntLon = 96.11666666666666, pntEle = Just 90.0, pntTime = Nothing}))
  , (WITM, ICAOData (Point {pntLat = 5.233333333333333, pntLon = 97.2, pntEle = Just 87.0, pntTime = Nothing}))
  , (WITT, ICAOData (Point {pntLat = 5.516666666666667, pntLon = 95.41666666666667, pntEle = Just 21.0, pntTime = Nothing}))
  , (WMBA, ICAOData (Point {pntLat = 4.216666666666667, pntLon = 100.7, pntEle = Just 7.0, pntTime = Nothing}))
  , (WMKC, ICAOData (Point {pntLat = 6.166666666666667, pntLon = 102.28333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (WMKD, ICAOData (Point {pntLat = 3.6166666666666667, pntLon = 103.21666666666667, pntEle = Just 18.0, pntTime = Nothing}))
  , (WMKJ, ICAOData (Point {pntLat = 1.6333333333333333, pntLon = 103.66666666666667, pntEle = Just 37.0, pntTime = Nothing}))
  , (WMKK, ICAOData (Point {pntLat = 2.7333333333333334, pntLon = 101.7, pntEle = Just 21.0, pntTime = Nothing}))
  , (WMKL, ICAOData (Point {pntLat = 6.333333333333333, pntLon = 99.73333333333333, pntEle = Just 8.0, pntTime = Nothing}))
  , (WMKM, ICAOData (Point {pntLat = 2.2666666666666666, pntLon = 102.25, pntEle = Just 11.0, pntTime = Nothing}))
  , (WMKP, ICAOData (Point {pntLat = 5.3, pntLon = 100.26666666666667, pntEle = Just 3.0, pntTime = Nothing}))
  , (WPDL, ICAOData (Point {pntLat = -8.566666666666666, pntLon = 125.56666666666666, pntEle = Just 6.0, pntTime = Nothing}))
  , (WPEC, ICAOData (Point {pntLat = -8.5, pntLon = 126.4, pntEle = Just 522.0, pntTime = Nothing}))
  , (WPOC, ICAOData (Point {pntLat = -9.2, pntLon = 124.36666666666666, pntEle = Just 5.0, pntTime = Nothing}))
  , (WRBB, ICAOData (Point {pntLat = -3.4333333333333336, pntLon = 114.75, pntEle = Just 20.0, pntTime = Nothing}))
  , (WRBI, ICAOData (Point {pntLat = -2.7, pntLon = 110.7, pntEle = Just 25.0, pntTime = Nothing}))
  , (WRBK, ICAOData (Point {pntLat = -3.4, pntLon = 116.21666666666667, pntEle = Just 18.0, pntTime = Nothing}))
  , (WRBM, ICAOData (Point {pntLat = -0.95, pntLon = 114.9, pntEle = Just 60.0, pntTime = Nothing}))
  , (WRBP, ICAOData (Point {pntLat = -1.0, pntLon = 114.0, pntEle = Just 27.0, pntTime = Nothing}))
  , (WRKC, ICAOData (Point {pntLat = -8.633333333333333, pntLon = 122.25, pntEle = Just 3.0, pntTime = Nothing}))
  , (WRKK, ICAOData (Point {pntLat = -10.166666666666666, pntLon = 123.66666666666667, pntEle = Just 108.0, pntTime = Nothing}))
  , (WRKL, ICAOData (Point {pntLat = -8.266666666666667, pntLon = 122.96666666666667, pntEle = Just 9.0, pntTime = Nothing}))
  , (WRKM, ICAOData (Point {pntLat = -8.216666666666667, pntLon = 124.56666666666666, pntEle = Just 12.0, pntTime = Nothing}))
  , (WRKR, ICAOData (Point {pntLat = -10.733333333333333, pntLon = 123.06666666666666, pntEle = Just 1.0, pntTime = Nothing}))
  , (WRKS, ICAOData (Point {pntLat = -10.5, pntLon = 121.83333333333333, pntEle = Just 26.0, pntTime = Nothing}))
  , (WRLB, ICAOData (Point {pntLat = 3.7333333333333334, pntLon = 115.68333333333334, pntEle = Just 550.0, pntTime = Nothing}))
  , (WRLG, ICAOData (Point {pntLat = 2.85, pntLon = 117.33333333333333, pntEle = Just 50.0, pntTime = Nothing}))
  , (WRLK, ICAOData (Point {pntLat = 2.1166666666666667, pntLon = 117.45, pntEle = Just 26.0, pntTime = Nothing}))
  , (WRLL, ICAOData (Point {pntLat = -1.2666666666666666, pntLon = 116.9, pntEle = Just 3.0, pntTime = Nothing}))
  , (WRLR, ICAOData (Point {pntLat = 3.3333333333333335, pntLon = 117.56666666666666, pntEle = Just 6.0, pntTime = Nothing}))
  , (WRLS, ICAOData (Point {pntLat = -0.6166666666666667, pntLon = 117.15, pntEle = Just 230.0, pntTime = Nothing}))
  , (WRRA, ICAOData (Point {pntLat = -8.533333333333333, pntLon = 116.06666666666666, pntEle = Just 3.0, pntTime = Nothing}))
  , (WRRB, ICAOData (Point {pntLat = -8.55, pntLon = 118.7, pntEle = Just 2.0, pntTime = Nothing}))
  , (WRRR, ICAOData (Point {pntLat = -8.75, pntLon = 115.16666666666667, pntEle = Just 1.0, pntTime = Nothing}))
  , (WRRS, ICAOData (Point {pntLat = -8.433333333333334, pntLon = 117.41666666666667, pntEle = Just 3.0, pntTime = Nothing}))
  , (WRRW, ICAOData (Point {pntLat = -9.666666666666666, pntLon = 120.33333333333333, pntEle = Just 12.0, pntTime = Nothing}))
  , (WRSJ, ICAOData (Point {pntLat = -7.366666666666666, pntLon = 112.76666666666667, pntEle = Just 3.0, pntTime = Nothing}))
  , (WRSP, ICAOData (Point {pntLat = -7.216666666666667, pntLon = 112.71666666666667, pntEle = Just 3.0, pntTime = Nothing}))
  , (WRSQ, ICAOData (Point {pntLat = -7.866666666666667, pntLon = 110.91666666666667, pntEle = Just 104.0, pntTime = Nothing}))
  , (WRSS, ICAOData (Point {pntLat = -7.216666666666667, pntLon = 113.71666666666667, pntEle = Just 3.0, pntTime = Nothing}))
  , (WSAP, ICAOData (Point {pntLat = 1.3666666666666667, pntLon = 103.91666666666667, pntEle = Just 18.0, pntTime = Nothing}))
  , (WSSS, ICAOData (Point {pntLat = 1.3666666666666667, pntLon = 103.98333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (YBAF, ICAOData (Point {pntLat = -27.566666666666666, pntLon = 153.0, pntEle = Just 23.0, pntTime = Nothing}))
  , (YBAM, ICAOData (Point {pntLat = -27.633333333333333, pntLon = 152.71666666666667, pntEle = Just 27.0, pntTime = Nothing}))
  , (YBAS, ICAOData (Point {pntLat = -23.8, pntLon = 133.88333333333333, pntEle = Just 545.0, pntTime = Nothing}))
  , (YBBN, ICAOData (Point {pntLat = -27.383333333333333, pntLon = 153.1, pntEle = Just 4.0, pntTime = Nothing}))
  , (YBCG, ICAOData (Point {pntLat = -28.166666666666668, pntLon = 153.5, pntEle = Just 6.0, pntTime = Nothing}))
  , (YBCS, ICAOData (Point {pntLat = -16.883333333333333, pntLon = 145.75, pntEle = Just 3.0, pntTime = Nothing}))
  , (YBCV, ICAOData (Point {pntLat = -26.4, pntLon = 146.26666666666668, pntEle = Just 306.0, pntTime = Nothing}))
  , (YBGL, ICAOData (Point {pntLat = -23.85, pntLon = 151.25, pntEle = Just 75.0, pntTime = Nothing}))
  , (YBLR, ICAOData (Point {pntLat = -23.433333333333334, pntLon = 144.26666666666668, pntEle = Just 192.0, pntTime = Nothing}))
  , (YBMA, ICAOData (Point {pntLat = -20.666666666666668, pntLon = 139.46666666666667, pntEle = Just 342.0, pntTime = Nothing}))
  , (YBMK, ICAOData (Point {pntLat = -21.116666666666667, pntLon = 149.2, pntEle = Just 30.0, pntTime = Nothing}))
  , (YBOK, ICAOData (Point {pntLat = -27.416666666666668, pntLon = 151.73333333333332, pntEle = Just 407.0, pntTime = Nothing}))
  , (YBPN, ICAOData (Point {pntLat = -20.483333333333334, pntLon = 148.53333333333333, pntEle = Just 25.0, pntTime = Nothing}))
  , (YBRK, ICAOData (Point {pntLat = -23.383333333333333, pntLon = 150.46666666666667, pntEle = Just 10.0, pntTime = Nothing}))
  , (YBRM, ICAOData (Point {pntLat = -17.95, pntLon = 122.21666666666667, pntEle = Just 17.0, pntTime = Nothing}))
  , (YBTL, ICAOData (Point {pntLat = -19.25, pntLon = 146.75, pntEle = Just 6.0, pntTime = Nothing}))
  , (YBWP, ICAOData (Point {pntLat = -12.633333333333333, pntLon = 141.88333333333333, pntEle = Just 12.0, pntTime = Nothing}))
  , (YDGV, ICAOData (Point {pntLat = -12.266666666666667, pntLon = 136.81666666666666, pntEle = Just 52.0, pntTime = Nothing}))
  , (YDTC, ICAOData (Point {pntLat = -19.633333333333333, pntLon = 134.16666666666666, pntEle = Just 375.0, pntTime = Nothing}))
  , (YDYL, ICAOData (Point {pntLat = -25.183333333333334, pntLon = 130.96666666666667, pntEle = Just 493.0, pntTime = Nothing}))
  , (YMAY, ICAOData (Point {pntLat = -36.06666666666667, pntLon = 146.95, pntEle = Just 165.0, pntTime = Nothing}))
  , (YMDV, ICAOData (Point {pntLat = -41.166666666666664, pntLon = 146.36666666666667, pntEle = Just 47.0, pntTime = Nothing}))
  , (YMEN, ICAOData (Point {pntLat = -37.733333333333334, pntLon = 145.4, pntEle = Just 76.0, pntTime = Nothing}))
  , (YMES, ICAOData (Point {pntLat = -38.1, pntLon = 147.13333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (YMHB, ICAOData (Point {pntLat = -42.833333333333336, pntLon = 147.48333333333332, pntEle = Just 4.0, pntTime = Nothing}))
  , (YMLT, ICAOData (Point {pntLat = -41.53333333333333, pntLon = 147.2, pntEle = Just 171.0, pntTime = Nothing}))
  , (YMLV, ICAOData (Point {pntLat = -37.85, pntLon = 144.73333333333332, pntEle = Just 18.0, pntTime = Nothing}))
  , (YMMB, ICAOData (Point {pntLat = -37.96666666666667, pntLon = 145.1, pntEle = Just 13.0, pntTime = Nothing}))
  , (YMMG, ICAOData (Point {pntLat = -37.733333333333334, pntLon = 140.78333333333333, pntEle = Just 65.0, pntTime = Nothing}))
  , (YMMI, ICAOData (Point {pntLat = -34.21666666666667, pntLon = 142.08333333333334, pntEle = Just 50.0, pntTime = Nothing}))
  , (YMML, ICAOData (Point {pntLat = -37.666666666666664, pntLon = 144.83333333333334, pntEle = Just 132.0, pntTime = Nothing}))
  , (YMMQ, ICAOData (Point {pntLat = -54.483333333333334, pntLon = 158.95, pntEle = Just 6.0, pntTime = Nothing}))
  , (YMWY, ICAOData (Point {pntLat = -40.983333333333334, pntLon = 145.71666666666667, pntEle = Just 11.0, pntTime = Nothing}))
  , (YPAD, ICAOData (Point {pntLat = -34.93333333333333, pntLon = 138.51666666666668, pntEle = Just 6.0, pntTime = Nothing}))
  , (YPAL, ICAOData (Point {pntLat = -34.93333333333333, pntLon = 117.8, pntEle = Just 71.0, pntTime = Nothing}))
  , (YPBH, ICAOData (Point {pntLat = -31.966666666666665, pntLon = 141.46666666666667, pntEle = Just 315.0, pntTime = Nothing}))
  , (YPCC, ICAOData (Point {pntLat = -12.183333333333334, pntLon = 96.81666666666666, pntEle = Just 3.0, pntTime = Nothing}))
  , (YPCD, ICAOData (Point {pntLat = -32.11666666666667, pntLon = 133.7, pntEle = Just 23.0, pntTime = Nothing}))
  , (YPDB, ICAOData (Point {pntLat = -17.3, pntLon = 123.61666666666666, pntEle = Just 8.0, pntTime = Nothing}))
  , (YPDN, ICAOData (Point {pntLat = -12.4, pntLon = 130.86666666666667, pntEle = Just 31.0, pntTime = Nothing}))
  , (YPEA, ICAOData (Point {pntLat = -31.666666666666668, pntLon = 116.01666666666667, pntEle = Just 45.0, pntTime = Nothing}))
  , (YPED, ICAOData (Point {pntLat = -34.7, pntLon = 138.61666666666667, pntEle = Just 20.0, pntTime = Nothing}))
  , (YPFT, ICAOData (Point {pntLat = -30.833333333333332, pntLon = 128.1, pntEle = Just 160.0, pntTime = Nothing}))
  , (YPGN, ICAOData (Point {pntLat = -28.783333333333335, pntLon = 114.7, pntEle = Just 37.0, pntTime = Nothing}))
  , (YPKG, ICAOData (Point {pntLat = -30.783333333333335, pntLon = 121.45, pntEle = Just 367.0, pntTime = Nothing}))
  , (YPKU, ICAOData (Point {pntLat = -15.766666666666667, pntLon = 128.7, pntEle = Just 44.0, pntTime = Nothing}))
  , (YPLC, ICAOData (Point {pntLat = -30.583333333333332, pntLon = 138.41666666666666, pntEle = Just 261.0, pntTime = Nothing}))
  , (YPLM, ICAOData (Point {pntLat = -22.233333333333334, pntLon = 114.08333333333333, pntEle = Just 5.0, pntTime = Nothing}))
  , (YPMR, ICAOData (Point {pntLat = -26.6, pntLon = 118.53333333333333, pntEle = Just 522.0, pntTime = Nothing}))
  , (YPPD, ICAOData (Point {pntLat = -20.1, pntLon = 119.56666666666666, pntEle = Just 9.0, pntTime = Nothing}))
  , (YPPF, ICAOData (Point {pntLat = -34.78333333333333, pntLon = 138.63333333333333, pntEle = Just 17.0, pntTime = Nothing}))
  , (YPPH, ICAOData (Point {pntLat = -31.933333333333334, pntLon = 115.95, pntEle = Just 20.0, pntTime = Nothing}))
  , (YPWR, ICAOData (Point {pntLat = -31.133333333333333, pntLon = 136.81666666666666, pntEle = Just 165.0, pntTime = Nothing}))
  , (YPXM, ICAOData (Point {pntLat = -10.433333333333334, pntLon = 105.68333333333334, pntEle = Just 279.0, pntTime = Nothing}))
  , (YSBK, ICAOData (Point {pntLat = -33.916666666666664, pntLon = 150.98333333333332, pntEle = Just 9.0, pntTime = Nothing}))
  , (YSCB, ICAOData (Point {pntLat = -35.3, pntLon = 149.18333333333334, pntEle = Just 575.0, pntTime = Nothing}))
  , (YSCH, ICAOData (Point {pntLat = -30.316666666666666, pntLon = 153.11666666666667, pntEle = Just 5.0, pntTime = Nothing}))
  , (YSCM, ICAOData (Point {pntLat = -36.233333333333334, pntLon = 149.08333333333334, pntEle = Just 778.0, pntTime = Nothing}))
  , (YSCN, ICAOData (Point {pntLat = -34.03333333333333, pntLon = 150.68333333333334, pntEle = Just 70.0, pntTime = Nothing}))
  , (YSDU, ICAOData (Point {pntLat = -32.2, pntLon = 148.56666666666666, pntEle = Just 275.0, pntTime = Nothing}))
  , (YSNF, ICAOData (Point {pntLat = -29.033333333333335, pntLon = 167.93333333333334, pntEle = Just 113.0, pntTime = Nothing}))
  , (YSNW, ICAOData (Point {pntLat = -34.95, pntLon = 150.53333333333333, pntEle = Just 122.0, pntTime = Nothing}))
  , (YSRI, ICAOData (Point {pntLat = -33.6, pntLon = 150.78333333333333, pntEle = Just 21.0, pntTime = Nothing}))
  , (YSSY, ICAOData (Point {pntLat = -33.95, pntLon = 151.18333333333334, pntEle = Just 6.0, pntTime = Nothing}))
  , (YSTW, ICAOData (Point {pntLat = -31.083333333333332, pntLon = 150.83333333333334, pntEle = Just 410.0, pntTime = Nothing}))
  , (YSWG, ICAOData (Point {pntLat = -35.15, pntLon = 147.45, pntEle = Just 221.0, pntTime = Nothing}))
  , (YSWM, ICAOData (Point {pntLat = -32.78333333333333, pntLon = 151.81666666666666, pntEle = Just 9.0, pntTime = Nothing}))
  , (ZBAA, ICAOData (Point {pntLat = 39.93333333333333, pntLon = 116.28333333333333, pntEle = Just 55.0, pntTime = Nothing}))
  , (ZBHH, ICAOData (Point {pntLat = 40.81666666666667, pntLon = 111.68333333333334, pntEle = Just 1065.0, pntTime = Nothing}))
  , (ZBYN, ICAOData (Point {pntLat = 37.78333333333333, pntLon = 112.55, pntEle = Just 779.0, pntTime = Nothing}))
  , (ZGCS, ICAOData (Point {pntLat = 28.2, pntLon = 113.08333333333333, pntEle = Just 46.0, pntTime = Nothing}))
  , (ZGGG, ICAOData (Point {pntLat = 23.166666666666668, pntLon = 113.33333333333333, pntEle = Just 8.0, pntTime = Nothing}))
  , (ZGHK, ICAOData (Point {pntLat = 20.033333333333335, pntLon = 110.35, pntEle = Just 15.0, pntTime = Nothing}))
  , (ZGKL, ICAOData (Point {pntLat = 25.333333333333332, pntLon = 110.3, pntEle = Just 166.0, pntTime = Nothing}))
  , (ZGNN, ICAOData (Point {pntLat = 22.816666666666666, pntLon = 108.35, pntEle = Just 73.0, pntTime = Nothing}))
  , (ZGOW, ICAOData (Point {pntLat = 23.4, pntLon = 116.68333333333334, pntEle = Just 3.0, pntTime = Nothing}))
  , (ZGSZ, ICAOData (Point {pntLat = 22.55, pntLon = 114.1, pntEle = Just 18.0, pntTime = Nothing}))
  , (ZGZJ, ICAOData (Point {pntLat = 21.216666666666665, pntLon = 110.4, pntEle = Just 28.0, pntTime = Nothing}))
  , (ZHCC, ICAOData (Point {pntLat = 34.71666666666667, pntLon = 113.65, pntEle = Just 111.0, pntTime = Nothing}))
  , (ZHHH, ICAOData (Point {pntLat = 30.616666666666667, pntLon = 114.13333333333334, pntEle = Just 23.0, pntTime = Nothing}))
  , (ZKKC, ICAOData (Point {pntLat = 40.666666666666664, pntLon = 129.2, pntEle = Just 19.0, pntTime = Nothing}))
  , (ZKPY, ICAOData (Point {pntLat = 39.03333333333333, pntLon = 125.78333333333333, pntEle = Just 36.0, pntTime = Nothing}))
  , (ZLIC, ICAOData (Point {pntLat = 38.483333333333334, pntLon = 106.21666666666667, pntEle = Just 1112.0, pntTime = Nothing}))
  , (ZLJQ, ICAOData (Point {pntLat = 39.766666666666666, pntLon = 98.48333333333333, pntEle = Just 1478.0, pntTime = Nothing}))
  , (ZLSN, ICAOData (Point {pntLat = 34.3, pntLon = 108.93333333333334, pntEle = Just 398.0, pntTime = Nothing}))
  , (ZLXN, ICAOData (Point {pntLat = 36.61666666666667, pntLon = 101.76666666666667, pntEle = Just 2262.0, pntTime = Nothing}))
  , (ZLYA, ICAOData (Point {pntLat = 36.6, pntLon = 109.5, pntEle = Just 959.0, pntTime = Nothing}))
  , (ZPPP, ICAOData (Point {pntLat = 25.016666666666666, pntLon = 102.68333333333334, pntEle = Just 1892.0, pntTime = Nothing}))
  , (ZSAM, ICAOData (Point {pntLat = 24.483333333333334, pntLon = 118.08333333333333, pntEle = Just 139.0, pntTime = Nothing}))
  , (ZSCN, ICAOData (Point {pntLat = 28.6, pntLon = 115.91666666666667, pntEle = Just 50.0, pntTime = Nothing}))
  , (ZSFZ, ICAOData (Point {pntLat = 26.083333333333332, pntLon = 119.28333333333333, pntEle = Just 85.0, pntTime = Nothing}))
  , (ZSGZ, ICAOData (Point {pntLat = 25.85, pntLon = 114.95, pntEle = Just 125.0, pntTime = Nothing}))
  , (ZSHC, ICAOData (Point {pntLat = 30.233333333333334, pntLon = 120.16666666666667, pntEle = Just 43.0, pntTime = Nothing}))
  , (ZSNJ, ICAOData (Point {pntLat = 32.0, pntLon = 118.8, pntEle = Just 12.0, pntTime = Nothing}))
  , (ZSOF, ICAOData (Point {pntLat = 31.866666666666667, pntLon = 117.23333333333333, pntEle = Just 36.0, pntTime = Nothing}))
  , (ZSQD, ICAOData (Point {pntLat = 36.06666666666667, pntLon = 120.33333333333333, pntEle = Just 77.0, pntTime = Nothing}))
  , (ZSSS, ICAOData (Point {pntLat = 31.166666666666668, pntLon = 121.43333333333334, pntEle = Just 7.0, pntTime = Nothing}))
  , (ZSTN, ICAOData (Point {pntLat = 36.68333333333333, pntLon = 116.98333333333333, pntEle = Just 58.0, pntTime = Nothing}))
  , (ZUCK, ICAOData (Point {pntLat = 29.516666666666666, pntLon = 106.48333333333333, pntEle = Just 351.0, pntTime = Nothing}))
  , (ZUGY, ICAOData (Point {pntLat = 26.583333333333332, pntLon = 106.71666666666667, pntEle = Just 1074.0, pntTime = Nothing}))
  , (ZULS, ICAOData (Point {pntLat = 29.666666666666668, pntLon = 91.13333333333334, pntEle = Just 3650.0, pntTime = Nothing}))
  , (ZUUU, ICAOData (Point {pntLat = 30.666666666666668, pntLon = 104.01666666666667, pntEle = Just 508.0, pntTime = Nothing}))
  , (ZWHM, ICAOData (Point {pntLat = 42.81666666666667, pntLon = 93.51666666666667, pntEle = Just 739.0, pntTime = Nothing}))
  , (ZWSH, ICAOData (Point {pntLat = 39.46666666666667, pntLon = 75.98333333333333, pntEle = Just 1291.0, pntTime = Nothing}))
  , (ZWTN, ICAOData (Point {pntLat = 37.13333333333333, pntLon = 79.93333333333334, pntEle = Just 1375.0, pntTime = Nothing}))
  , (ZWYN, ICAOData (Point {pntLat = 43.95, pntLon = 81.33333333333333, pntEle = Just 663.0, pntTime = Nothing}))
  , (ZYCC, ICAOData (Point {pntLat = 43.9, pntLon = 125.21666666666667, pntEle = Just 238.0, pntTime = Nothing}))
  , (ZYQQ, ICAOData (Point {pntLat = 47.38333333333333, pntLon = 123.91666666666667, pntEle = Just 148.0, pntTime = Nothing}))
  , (ZYTL, ICAOData (Point {pntLat = 38.9, pntLon = 121.63333333333334, pntEle = Just 97.0, pntTime = Nothing}))
  ]
