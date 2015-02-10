{-# LANGUAGE OverloadedStrings #-}
-- naming convention: kmip_<KMIP VERSION>__<SECTION VERSION>_<REQUEST/RESPONSE>
module Spec where

import Ttlv.Tag
import Ttlv.Data
import Ttlv.Parser

import Test.Hspec

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base16 as B16

fromHex x = L.fromChunks [fst $ B16.decode x]

kmip_1_0__3_1_1_create_request = fromHex "42007801000001204200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F01000000D842005C0500000004000000010000000042007901000000C04200570500000004000000020000000042009101000000A8420008010000003042000A070000001743727970746F6772617068696320416C676F726974686D0042000B05000000040000000300000000420008010000003042000A070000001443727970746F67726170686963204C656E6774680000000042000B02000000040000008000000000420008010000003042000A070000001843727970746F67726170686963205573616765204D61736B42000B02000000040000000C00000000"
kmip_1_0__3_1_1_create_response = fromHex "42007B01000000C042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBE7C242000D0200000004000000010000000042000F010000006842005C0500000004000000010000000042007F0500000004000000000000000042007C010000004042005705000000040000000200000000420094070000002466633838333364652D373064322D346563652D623036332D66656465336133633539666500000000"
kmip_1_0__3_1_1_destroy_request = fromHex "42007801000000904200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000004842005C050000000400000014000000004200790100000030420094070000002466633838333364652D373064322D346563652D623036332D66656465336133633539666500000000"
kmip_1_0__3_1_1_destroy_response = fromHex "42007B01000000B042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBE7C342000D0200000004000000010000000042000F010000005842005C0500000004000000140000000042007F0500000004000000000000000042007C0100000030420094070000002466633838333364652D373064322D346563652D623036332D66656465336133633539666500000000"


kmip_1_0__3_1_2_register_request = fromHex "42007801000001C84200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000018042005C0500000004000000030000000042007901000001684200570500000004000000060000000042009101000000004200900100000148420008010000002842000A070000000C4F626A6563742047726F75700000000042000B070000000647726F7570310000420008010000005842000A07000000204170706C69636174696F6E20537065636966696320496E666F726D6174696F6E42000B0100000028420003070000000373736C0000000000420002070000000F7777772E6578616D706C652E636F6D00420008010000003042000A0700000013436F6E7461637420496E666F726D6174696F6E000000000042000B07000000034A6F650000000000420008010000003042000A0700000009782D507572706F73650000000000000042000B070000000D64656D6F6E7374726174696F6E000000420008010000004042000A07000000044E616D650000000042000B0100000028420055070000000954656D706C617465310000000000000042005405000000040000000100000000"
kmip_1_0__3_1_2_register_response = fromHex "42007B01000000B042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBE7C442000D0200000004000000010000000042000F010000005842005C0500000004000000030000000042007F0500000004000000000000000042007C0100000030420094070000002461366562626236662D346335342D346262622D616432392D62653662616434656361643500000000"
kmip_1_0__3_1_2_create_request = fromHex "42007801000001504200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000010842005C0500000004000000010000000042007901000000F04200570500000004000000020000000042009101000000D84200530100000028420055070000000954656D706C617465310000000000000042005405000000040000000100000000420008010000003042000A070000001743727970746F6772617068696320416C676F726974686D0042000B05000000040000000300000000420008010000003042000A070000001443727970746F67726170686963204C656E6774680000000042000B02000000040000008000000000420008010000003042000A070000001843727970746F67726170686963205573616765204D61736B42000B02000000040000000C00000000"
kmip_1_0__3_1_2_create_response = fromHex "42007B01000000C042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBE7C542000D0200000004000000010000000042000F010000006842005C0500000004000000010000000042007F0500000004000000000000000042007C010000004042005705000000040000000200000000420094070000002436316231303631342D643862352D343666392D386431372D32666136656131643734376100000000"
kmip_1_0__3_1_2_get_attributes_request = fromHex "42007801000001084200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F01000000C042005C05000000040000000B0000000042007901000000A8420094070000002436316231303631342D643862352D343666392D386431372D3266613665613164373437610000000042000A070000000C4F626A6563742047726F75700000000042000A07000000204170706C69636174696F6E20537065636966696320496E666F726D6174696F6E42000A0700000013436F6E7461637420496E666F726D6174696F6E000000000042000A0700000009782D507572706F736500000000000000"
kmip_1_0__3_1_2_get_attributes_response = fromHex "42007B01000001B042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBE7C642000D0200000004000000010000000042000F010000015842005C05000000040000000B0000000042007F0500000004000000000000000042007C0100000130420094070000002436316231303631342D643862352D343666392D386431372D32666136656131643734376100000000420008010000002842000A070000000C4F626A6563742047726F75700000000042000B070000000647726F7570310000420008010000005842000A07000000204170706C69636174696F6E20537065636966696320496E666F726D6174696F6E42000B0100000028420003070000000373736C0000000000420002070000000F7777772E6578616D706C652E636F6D00420008010000003042000A0700000013436F6E7461637420496E666F726D6174696F6E000000000042000B07000000034A6F650000000000420008010000003042000A0700000009782D507572706F73650000000000000042000B070000000D64656D6F6E7374726174696F6E000000"
kmip_1_0__3_1_2_destroy_request1 = fromHex "42007801000000904200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000004842005C050000000400000014000000004200790100000030420094070000002436316231303631342D643862352D343666392D386431372D32666136656131643734376100000000"
kmip_1_0__3_1_2_destroy_response1 = fromHex "42007B01000000B042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBE7C642000D0200000004000000010000000042000F010000005842005C0500000004000000140000000042007F0500000004000000000000000042007C0100000030420094070000002436316231303631342D643862352D343666392D386431372D32666136656131643734376100000000"
kmip_1_0__3_1_2_destroy_request2 = fromHex "42007801000000904200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000004842005C050000000400000014000000004200790100000030420094070000002461366562626236662D346335342D346262622D616432392D62653662616434656361643500000000"
kmip_1_0__3_1_2_destroy_response2 = fromHex "42007B01000000B042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBE7C642000D0200000004000000010000000042000F010000005842005C0500000004000000140000000042007F0500000004000000000000000042007C0100000030420094070000002461366562626236662D346335342D346262622D616432392D62653662616434656361643500000000"

kmip_1_0__3_1_3_create_request = fromHex "42007801000001984200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000015042005C050000000400000001000000004200790100000138420057050000000400000002000000004200910100000120420008010000003842000A07000000044E616D650000000042000B010000002042005507000000044B6579310000000042005405000000040000000100000000420008010000003042000A070000001743727970746F6772617068696320416C676F726974686D0042000B05000000040000000200000000420008010000003042000A070000001443727970746F67726170686963204C656E6774680000000042000B0200000004000000A800000000420008010000003042000A070000001843727970746F67726170686963205573616765204D61736B42000B02000000040000000C00000000420008010000003042000A0700000013436F6E7461637420496E666F726D6174696F6E000000000042000B07000000034A6F650000000000"
kmip_1_0__3_1_3_create_response = fromHex "42007B01000000C042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBE7C742000D0200000004000000010000000042000F010000006842005C0500000004000000010000000042007F0500000004000000000000000042007C010000004042005705000000040000000200000000420094070000002431656432386561352D326233312D343134352D626366322D33366430373536643338393000000000"
kmip_1_0__3_1_3_locate_request1 = fromHex "42007801000000D04200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000008842005C050000000400000008000000004200790100000070420008010000002842000A070000000B4F626A6563742054797065000000000042000B05000000040000000200000000420008010000003842000A07000000044E616D650000000042000B010000002042005507000000044B6579310000000042005405000000040000000100000000"
kmip_1_0__3_1_3_locate_response1 = fromHex "42007B01000000B042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBE7C842000D0200000004000000010000000042000F010000005842005C0500000004000000080000000042007F0500000004000000000000000042007C0100000030420094070000002431656432386561352D326233312D343134352D626366322D33366430373536643338393000000000"
kmip_1_0__3_1_3_get_request = fromHex "42007801000000904200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000004842005C05000000040000000A000000004200790100000030420094070000002431656432386561352D326233312D343134352D626366322D33366430373536643338393000000000"
kmip_1_0__3_1_3_get_response = fromHex "42007B010000012842007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBE7C842000D0200000004000000010000000042000F01000000D042005C05000000040000000A0000000042007F0500000004000000000000000042007C01000000A842005705000000040000000200000000420094070000002431656432386561352D326233312D343134352D626366322D3336643037353664333839300000000042008F010000006042004001000000584200420500000004000000010000000042004501000000204200430800000018C8E51523F73D6EE9F40EAB7CD06825499D8C0BD0739E10464200280500000004000000020000000042002A0200000004000000A800000000"
kmip_1_0__3_1_3_destroy_request = fromHex "42007801000000904200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000004842005C050000000400000014000000004200790100000030420094070000002431656432386561352D326233312D343134352D626366322D33366430373536643338393000000000"
kmip_1_0__3_1_3_destroy_response = fromHex "42007B01000000B042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBE7C842000D0200000004000000010000000042000F010000005842005C0500000004000000140000000042007F0500000004000000000000000042007C0100000030420094070000002431656432386561352D326233312D343134352D626366322D33366430373536643338393000000000"
kmip_1_0__3_1_3_locate_request2 = fromHex "42007801000000B84200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000007042005C050000000400000008000000004200790100000058420008010000005042000A0700000011556E69717565204964656E7469666965720000000000000042000B070000002431656432386561352D326233312D343134352D626366322D33366430373536643338393000000000"
kmip_1_0__3_1_3_locate_response2 = fromHex "42007B010000008042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBE7C842000D0200000004000000010000000042000F010000002842005C0500000004000000080000000042007F0500000004000000000000000042007C0100000000"

kmip_1_0__3_1_4_a_register_request = fromHex "42007801000001384200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F01000000F042005C0500000004000000030000000042007901000000D842005705000000040000000600000000420091010000000042009001000000B8420008010000003042000A070000001743727970746F6772617068696320416C676F726974686D0042000B05000000040000000300000000420008010000003042000A070000001443727970746F67726170686963204C656E6774680000000042000B02000000040000008000000000420008010000004042000A07000000044E616D650000000042000B0100000028420055070000000954656D706C617465310000000000000042005405000000040000000100000000"
kmip_1_0__3_1_4_a_register_response = fromHex "42007B01000000B042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBED2142000D0200000004000000010000000042000F010000005842005C0500000004000000030000000042007F0500000004000000000000000042007C0100000030420094070000002434356438363239612D396164312D343162332D396430392D39343166326135393564613300000000"
kmip_1_0__3_1_4_a_create_request = fromHex "42007801000001584200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000011042005C0500000004000000010000000042007901000000F84200570500000004000000020000000042009101000000E04200530100000028420055070000000954656D706C617465310000000000000042005405000000040000000100000000420008010000003842000A07000000044E616D650000000042000B010000002042005507000000044B6579310000000042005405000000040000000100000000420008010000003042000A070000001843727970746F67726170686963205573616765204D61736B42000B02000000040000000400000000420008010000003042000A0700000013436F6E7461637420496E666F726D6174696F6E000000000042000B0700000003466F6F0000000000"
kmip_1_0__3_1_4_a_create_response = fromHex "42007B01000000C042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBED2342000D0200000004000000010000000042000F010000006842005C0500000004000000010000000042007F0500000004000000000000000042007C010000004042005705000000040000000200000000420094070000002430613333653833652D356237612D343836352D393634612D38643163336262663961653300000000"
kmip_1_0__3_1_4_b_locate_request = fromHex "42007801000001204200770100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200100600000008000000000000000142000D0200000004000000020000000042000F010000009842005C0500000004000000080000000042009308000000080E9E1875336E415E4200790100000070420008010000002842000A070000000B4F626A6563742054797065000000000042000B05000000040000000200000000420008010000003842000A07000000044E616D650000000042000B010000002042005507000000044B657931000000004200540500000004000000010000000042000F010000002842005C05000000040000000A000000004200930800000008CFEF21DDDF1CF5E34200790100000000"
kmip_1_0__3_1_4_b_locate_response = fromHex "42007B01000001A042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBED2442000D0200000004000000020000000042000F010000006842005C0500000004000000080000000042009308000000080E9E1875336E415E42007F0500000004000000000000000042007C0100000030420094070000002430613333653833652D356237612D343836352D393634612D3864316333626266396165330000000042000F01000000D842005C05000000040000000A000000004200930800000008CFEF21DDDF1CF5E342007F0500000004000000000000000042007C01000000A042005705000000040000000200000000420094070000002430613333653833652D356237612D343836352D393634612D3864316333626266396165330000000042008F010000005842004001000000504200420500000004000000010000000042004501000000184200430800000010755D03C639648FB5828D5F1CC9FE9B574200280500000004000000030000000042002A02000000040000008000000000"
kmip_1_0__3_1_4_b_get_attribute_list_request = fromHex "42007801000000904200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000004842005C05000000040000000C000000004200790100000030420094070000002430613333653833652D356237612D343836352D393634612D38643163336262663961653300000000"
kmip_1_0__3_1_4_b_get_attribute_list_response = fromHex "42007B01000001C842007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBED2442000D0200000004000000010000000042000F010000017042005C05000000040000000C0000000042007F0500000004000000000000000042007C0100000148420094070000002430613333653833652D356237612D343836352D393634612D3864316333626266396165330000000042000A070000001443727970746F67726170686963204C656E6774680000000042000A070000001743727970746F6772617068696320416C676F726974686D0042000A0700000005537461746500000042000A0700000006446967657374000042000A070000000C496E697469616C20446174650000000042000A0700000011556E69717565204964656E7469666965720000000000000042000A07000000044E616D650000000042000A070000001843727970746F67726170686963205573616765204D61736B42000A070000000B4F626A6563742054797065000000000042000A0700000013436F6E7461637420496E666F726D6174696F6E000000000042000A07000000104C617374204368616E67652044617465"
kmip_1_0__3_1_4_b_get_attributes_request = fromHex "42007801000000C04200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000007842005C05000000040000000B000000004200790100000060420094070000002430613333653833652D356237612D343836352D393634612D3864316333626266396165330000000042000A07000000044E616D650000000042000A0700000013436F6E7461637420496E666F726D6174696F6E0000000000"
kmip_1_0__3_1_4_b_get_attributes_response = fromHex "42007B010000012842007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBED2442000D0200000004000000010000000042000F01000000D042005C05000000040000000B0000000042007F0500000004000000000000000042007C01000000A8420094070000002430613333653833652D356237612D343836352D393634612D38643163336262663961653300000000420008010000003842000A07000000044E616D650000000042000B010000002042005507000000044B6579310000000042005405000000040000000100000000420008010000003042000A0700000013436F6E7461637420496E666F726D6174696F6E000000000042000B0700000003466F6F0000000000"
kmip_1_0__3_1_4_b_add_attribute_request = fromHex "42007801000001604200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000020000000042000F010000008842005C05000000040000000D0000000042009308000000087A92DDA525EB158A4200790100000060420094070000002430613333653833652D356237612D343836352D393634612D38643163336262663961653300000000420008010000002842000A070000000C782D617474726962757465310000000042000B070000000656616C756531000042000F010000008842005C05000000040000000D0000000042009308000000087230F6E4D3BEA2494200790100000060420094070000002430613333653833652D356237612D343836352D393634612D38643163336262663961653300000000420008010000002842000A070000000C782D617474726962757465320000000042000B070000000656616C7565320000"
kmip_1_0__3_1_4_b_add_attribute_response = fromHex "42007B010000019042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBED2542000D0200000004000000020000000042000F010000009842005C05000000040000000D0000000042009308000000087A92DDA525EB158A42007F0500000004000000000000000042007C0100000060420094070000002430613333653833652D356237612D343836352D393634612D38643163336262663961653300000000420008010000002842000A070000000C782D617474726962757465310000000042000B070000000656616C756531000042000F010000009842005C05000000040000000D0000000042009308000000087230F6E4D3BEA24942007F0500000004000000000000000042007C0100000060420094070000002430613333653833652D356237612D343836352D393634612D38643163336262663961653300000000420008010000002842000A070000000C782D617474726962757465320000000042000B070000000656616C7565320000"
kmip_1_0__3_1_4_b_modify_attribute_request = fromHex "42007801000001704200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000020000000042000F010000009042005C05000000040000000E000000004200930800000008BA3EA60548ECB6994200790100000068420094070000002430613333653833652D356237612D343836352D393634612D38643163336262663961653300000000420008010000003042000A070000000C782D617474726962757465310000000042000B070000000E4D6F64696669656456616C756531000042000F010000009042005C05000000040000000E000000004200930800000008321984E716274A3D4200790100000068420094070000002430613333653833652D356237612D343836352D393634612D38643163336262663961653300000000420008010000003042000A070000000C782D617474726962757465320000000042000B070000000E4D6F64696669656456616C7565320000"
kmip_1_0__3_1_4_b_modify_attribute_response = fromHex "42007B01000001A042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBED2642000D0200000004000000020000000042000F01000000A042005C05000000040000000E000000004200930800000008BA3EA60548ECB69942007F0500000004000000000000000042007C0100000068420094070000002430613333653833652D356237612D343836352D393634612D38643163336262663961653300000000420008010000003042000A070000000C782D617474726962757465310000000042000B070000000E4D6F64696669656456616C756531000042000F01000000A042005C05000000040000000E000000004200930800000008321984E716274A3D42007F0500000004000000000000000042007C0100000068420094070000002430613333653833652D356237612D343836352D393634612D38643163336262663961653300000000420008010000003042000A070000000C782D617474726962757465320000000042000B070000000E4D6F64696669656456616C7565320000"
kmip_1_0__3_1_4_b_delete_attribute_request = fromHex "42007801000001304200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000020000000042000F010000007042005C05000000040000000F000000004200930800000008D5C6DF842DAEECD84200790100000048420094070000002430613333653833652D356237612D343836352D393634612D3864316333626266396165330000000042000A070000000C782D617474726962757465310000000042000F010000007042005C05000000040000000F000000004200930800000008572D4F0D433DAB104200790100000048420094070000002430613333653833652D356237612D343836352D393634612D3864316333626266396165330000000042000A070000000C782D6174747269627574653200000000"
kmip_1_0__3_1_4_b_delete_attribute_response = fromHex "42007B01000001A042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBED2642000D0200000004000000020000000042000F01000000A042005C05000000040000000F000000004200930800000008D5C6DF842DAEECD842007F0500000004000000000000000042007C0100000068420094070000002430613333653833652D356237612D343836352D393634612D38643163336262663961653300000000420008010000003042000A070000000C782D617474726962757465310000000042000B070000000E4D6F64696669656456616C756531000042000F01000000A042005C05000000040000000F000000004200930800000008572D4F0D433DAB1042007F0500000004000000000000000042007C0100000068420094070000002430613333653833652D356237612D343836352D393634612D38643163336262663961653300000000420008010000003042000A070000000C782D617474726962757465320000000042000B070000000E4D6F64696669656456616C7565320000"
kmip_1_0__3_1_4_a_destroy_request1 = fromHex "42007801000000904200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000004842005C050000000400000014000000004200790100000030420094070000002430613333653833652D356237612D343836352D393634612D38643163336262663961653300000000"
kmip_1_0__3_1_4_a_destroy_response1 = fromHex "42007B01000000B042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBED2742000D0200000004000000010000000042000F010000005842005C0500000004000000140000000042007F0500000004000000000000000042007C0100000030420094070000002430613333653833652D356237612D343836352D393634612D38643163336262663961653300000000"
kmip_1_0__3_1_4_a_get_request1 = fromHex "42007801000000904200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000004842005C05000000040000000A000000004200790100000030420094070000002430613333653833652D356237612D343836352D393634612D38643163336262663961653300000000"
kmip_1_0__3_1_4_a_get_response1 = fromHex "42007B01000000A842007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBED2742000D0200000004000000010000000042000F010000005042005C05000000040000000A0000000042007F0500000004000000010000000042007E0500000004000000010000000042007D07000000154F626A65637420646F6573206E6F74206578697374000000"
kmip_1_0__3_1_4_a_destroy_request2 = fromHex "42007801000000904200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000004842005C050000000400000014000000004200790100000030420094070000002434356438363239612D396164312D343162332D396430392D39343166326135393564613300000000"
kmip_1_0__3_1_4_a_destroy_response2 = fromHex "42007B01000000B042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBED2742000D0200000004000000010000000042000F010000005842005C0500000004000000140000000042007F0500000004000000000000000042007C0100000030420094070000002434356438363239612D396164312D343162332D396430392D39343166326135393564613300000000"
kmip_1_0__3_1_4_a_get_request2 = fromHex "42007801000000904200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000004842005C05000000040000000A000000004200790100000030420094070000002434356438363239612D396164312D343162332D396430392D39343166326135393564613300000000"
kmip_1_0__3_1_4_a_get_response2 = fromHex "42007B01000000D042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004AFBED2742000D0200000004000000010000000042000F010000007842005C05000000040000000A0000000042007F0500000004000000010000000042007E0500000004000000010000000042007D070000003A4E6F2043727970746F67726170686963204F626A65637420666F756E64207769746820676976656E20556E69717565204964656E746966696572000000000000"


kmip_1_0__3_1_5_register_request = fromHex "42007801000001004200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F01000000B842005C0500000004000000030000000042007901000000A0420057050000000400000007000000004200910100000038420008010000003042000A070000001843727970746F67726170686963205573616765204D61736B42000B020000000400000002000000004200850100000048420086050000000400000001000000004200400100000030420042050000000400000002000000004200450100000018420043080000000E53656372657450617373776F72640000"
kmip_1_0__3_1_5_register_response = fromHex "42007B01000000B042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004B7924D142000D0200000004000000010000000042000F010000005842005C0500000004000000030000000042007F0500000004000000000000000042007C0100000030420094070000002433393632326363322D653564342D346461392D396631302D33626466363462306537363000000000"
kmip_1_0__3_1_5_destroy_request = fromHex "42007801000000904200770100000038420069010000002042006A0200000004000000010000000042006B0200000004000000000000000042000D0200000004000000010000000042000F010000004842005C050000000400000014000000004200790100000030420094070000002433393632326363322D653564342D346461392D396631302D33626466363462306537363000000000"
kmip_1_0__3_1_5_destroy_response = fromHex "42007B01000000B042007A0100000048420069010000002042006A0200000004000000010000000042006B020000000400000000000000004200920900000008000000004B7924D142000D0200000004000000010000000042000F010000005842005C0500000004000000140000000042007F0500000004000000000000000042007C0100000030420094070000002433393632326363322D653564342D346461392D396631302D33626466363462306537363000000000"

test :: IO ()
test = hspec spec

xxx = do
  putStrLn $ show $ decodeTtlv kmip_1_0__3_1_2_register_request

spec :: Spec
spec = do
  describe "Use Cases" $ do
    describe "1.0" $ do
      describe "3.1 Basic functionality" $ do
        it "3.1.1 Create / Destroy" $ do
          encodeTtlv (decodeTtlv kmip_1_0__3_1_1_create_request) `shouldBe` kmip_1_0__3_1_1_create_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_1_create_response) `shouldBe` kmip_1_0__3_1_1_create_response
          encodeTtlv (decodeTtlv kmip_1_0__3_1_1_destroy_request) `shouldBe` kmip_1_0__3_1_1_destroy_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_1_destroy_response) `shouldBe` kmip_1_0__3_1_1_destroy_response

        it "3.1.2 Register / Create / Get Attributes / Destroy" $ do
          encodeTtlv (decodeTtlv kmip_1_0__3_1_2_register_request) `shouldBe` kmip_1_0__3_1_2_register_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_2_register_response) `shouldBe` kmip_1_0__3_1_2_register_response
          encodeTtlv (decodeTtlv kmip_1_0__3_1_2_create_request) `shouldBe` kmip_1_0__3_1_2_create_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_2_create_response) `shouldBe` kmip_1_0__3_1_2_create_response
          encodeTtlv (decodeTtlv kmip_1_0__3_1_2_get_attributes_request) `shouldBe`  kmip_1_0__3_1_2_get_attributes_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_2_get_attributes_response) `shouldBe`  kmip_1_0__3_1_2_get_attributes_response
          encodeTtlv (decodeTtlv kmip_1_0__3_1_2_destroy_request1) `shouldBe` kmip_1_0__3_1_2_destroy_request1
          encodeTtlv (decodeTtlv kmip_1_0__3_1_2_destroy_response1) `shouldBe` kmip_1_0__3_1_2_destroy_response1

        it "3.1.3 Create / Locate / Get / Destroy" $ do
          encodeTtlv (decodeTtlv kmip_1_0__3_1_3_create_request) `shouldBe` kmip_1_0__3_1_3_create_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_3_create_response) `shouldBe` kmip_1_0__3_1_3_create_response
          encodeTtlv (decodeTtlv kmip_1_0__3_1_3_locate_request1) `shouldBe` kmip_1_0__3_1_3_locate_request1
          encodeTtlv (decodeTtlv kmip_1_0__3_1_3_locate_response1) `shouldBe` kmip_1_0__3_1_3_locate_response1
          encodeTtlv (decodeTtlv kmip_1_0__3_1_3_get_request) `shouldBe` kmip_1_0__3_1_3_get_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_3_get_response) `shouldBe` kmip_1_0__3_1_3_get_response
          encodeTtlv (decodeTtlv kmip_1_0__3_1_3_destroy_request) `shouldBe` kmip_1_0__3_1_3_destroy_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_3_destroy_response) `shouldBe` kmip_1_0__3_1_3_destroy_response
          encodeTtlv (decodeTtlv kmip_1_0__3_1_3_locate_request2) `shouldBe` kmip_1_0__3_1_3_locate_request2
          encodeTtlv (decodeTtlv kmip_1_0__3_1_3_locate_response2) `shouldBe` kmip_1_0__3_1_3_locate_response2

        it "3.1.4 Dual-client use case, ID Placeholder linked Locate & Get batch" $ do
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_a_register_request) `shouldBe` kmip_1_0__3_1_4_a_register_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_a_register_response) `shouldBe` kmip_1_0__3_1_4_a_register_response
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_a_create_request) `shouldBe` kmip_1_0__3_1_4_a_create_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_a_create_response) `shouldBe` kmip_1_0__3_1_4_a_create_response
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_b_locate_request) `shouldBe` kmip_1_0__3_1_4_b_locate_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_b_locate_response) `shouldBe` kmip_1_0__3_1_4_b_locate_response
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_b_get_attribute_list_request) `shouldBe` kmip_1_0__3_1_4_b_get_attribute_list_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_b_get_attribute_list_response) `shouldBe` kmip_1_0__3_1_4_b_get_attribute_list_response
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_b_get_attributes_request) `shouldBe` kmip_1_0__3_1_4_b_get_attributes_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_b_get_attributes_response) `shouldBe` kmip_1_0__3_1_4_b_get_attributes_response
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_b_add_attribute_request) `shouldBe` kmip_1_0__3_1_4_b_add_attribute_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_b_add_attribute_response) `shouldBe` kmip_1_0__3_1_4_b_add_attribute_response
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_b_modify_attribute_request) `shouldBe` kmip_1_0__3_1_4_b_modify_attribute_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_b_modify_attribute_response) `shouldBe` kmip_1_0__3_1_4_b_modify_attribute_response
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_b_delete_attribute_request) `shouldBe` kmip_1_0__3_1_4_b_delete_attribute_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_b_delete_attribute_response) `shouldBe` kmip_1_0__3_1_4_b_delete_attribute_response
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_a_destroy_request1) `shouldBe` kmip_1_0__3_1_4_a_destroy_request1
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_a_destroy_response1) `shouldBe` kmip_1_0__3_1_4_a_destroy_response1
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_a_get_request1) `shouldBe` kmip_1_0__3_1_4_a_get_request1
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_a_get_response1) `shouldBe` kmip_1_0__3_1_4_a_get_response1
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_a_destroy_request2) `shouldBe` kmip_1_0__3_1_4_a_destroy_request2
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_a_destroy_response2) `shouldBe` kmip_1_0__3_1_4_a_destroy_response2
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_a_get_request2) `shouldBe` kmip_1_0__3_1_4_a_get_request2
          encodeTtlv (decodeTtlv kmip_1_0__3_1_4_a_get_response2) `shouldBe` kmip_1_0__3_1_4_a_get_response2

        it "3.1.5 Register / Destroy Secret Data" $ do
          encodeTtlv (decodeTtlv kmip_1_0__3_1_5_register_request) `shouldBe` kmip_1_0__3_1_5_register_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_5_register_response) `shouldBe` kmip_1_0__3_1_5_register_response
          encodeTtlv (decodeTtlv kmip_1_0__3_1_5_destroy_request) `shouldBe` kmip_1_0__3_1_5_destroy_request
          encodeTtlv (decodeTtlv kmip_1_0__3_1_5_destroy_response) `shouldBe` kmip_1_0__3_1_5_destroy_response