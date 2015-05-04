{-# LANGUAGE OverloadedStrings #-}
-- naming convention: kmip_<KMIP VERSION>__<SECTION VERSION>_<OPERATION>_<REQUEST/RESPONSE>
module Kmip10Spec where

import Ttlv.Tag
import Ttlv.Data
import Ttlv.Parser

import Test.Hspec

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base16 as B16

import Ttlv.Validator.Message
import Ttlv.Validator.Objects
import Ttlv.Validator.Structures

import Kmip10Data

spec :: Spec
spec = do
  describe "Use Cases" $ do
    describe "1.0" $ do
      describe "3.1 Basic functionality" $ do
        let runIt x = encodeTtlv (decodeTtlv x) `shouldBe` x
        it "3.1.1 Create / Destroy" $ do
          runIt kmip_1_0__3_1_1_create_request
          runIt kmip_1_0__3_1_1_create_response
          runIt kmip_1_0__3_1_1_destroy_request
          runIt kmip_1_0__3_1_1_destroy_response

        it "3.1.2 Register / Create / Get Attributes / Destroy" $ do
          runIt kmip_1_0__3_1_2_register_request
          runIt kmip_1_0__3_1_2_register_response
          runIt kmip_1_0__3_1_2_create_request
          runIt kmip_1_0__3_1_2_create_response
          runIt kmip_1_0__3_1_2_get_attributes_request
          runIt kmip_1_0__3_1_2_get_attributes_response
          runIt kmip_1_0__3_1_2_destroy_request1
          runIt kmip_1_0__3_1_2_destroy_response1

        it "3.1.3 Create / Locate / Get / Destroy" $ do
          runIt kmip_1_0__3_1_3_create_request
          runIt kmip_1_0__3_1_3_create_response
          runIt kmip_1_0__3_1_3_locate_request1
          runIt kmip_1_0__3_1_3_locate_response1
          runIt kmip_1_0__3_1_3_get_request
          runIt kmip_1_0__3_1_3_get_response
          runIt kmip_1_0__3_1_3_destroy_request
          runIt kmip_1_0__3_1_3_destroy_response
          runIt kmip_1_0__3_1_3_locate_request2
          runIt kmip_1_0__3_1_3_locate_response2

        it "3.1.4 Dual-client use case, ID Placeholder linked Locate & Get batch" $ do
          runIt kmip_1_0__3_1_4_a_register_request
          runIt kmip_1_0__3_1_4_a_register_response
          runIt kmip_1_0__3_1_4_a_create_request
          runIt kmip_1_0__3_1_4_a_create_response
          runIt kmip_1_0__3_1_4_b_locate_request
          runIt kmip_1_0__3_1_4_b_locate_response
          runIt kmip_1_0__3_1_4_b_get_attribute_list_request
          runIt kmip_1_0__3_1_4_b_get_attribute_list_response
          runIt kmip_1_0__3_1_4_b_get_attributes_request
          runIt kmip_1_0__3_1_4_b_get_attributes_response
          runIt kmip_1_0__3_1_4_b_add_attribute_request
          runIt kmip_1_0__3_1_4_b_add_attribute_response
          runIt kmip_1_0__3_1_4_b_modify_attribute_request
          runIt kmip_1_0__3_1_4_b_modify_attribute_response
          runIt kmip_1_0__3_1_4_b_delete_attribute_request
          runIt kmip_1_0__3_1_4_b_delete_attribute_response
          runIt kmip_1_0__3_1_4_a_destroy_request1
          runIt kmip_1_0__3_1_4_a_destroy_response1
          runIt kmip_1_0__3_1_4_a_get_request1
          runIt kmip_1_0__3_1_4_a_get_response1
          runIt kmip_1_0__3_1_4_a_destroy_request2
          runIt kmip_1_0__3_1_4_a_destroy_response2
          runIt kmip_1_0__3_1_4_a_get_request2
          runIt kmip_1_0__3_1_4_a_get_response2

        it "3.1.5 Register / Destroy Secret Data" $ do
          runIt kmip_1_0__3_1_5_register_request
          runIt kmip_1_0__3_1_5_register_response
          runIt kmip_1_0__3_1_5_destroy_request
          runIt kmip_1_0__3_1_5_destroy_response

    describe "1.0 Validation" $ do
      let runIt x y = let j = decodeTtlv y
                      in runTtlvParser x j `shouldBe` Right j
      it "3.1.1" $ do
        runIt requestMessage kmip_1_0__3_1_1_create_request
        runIt responseMessage kmip_1_0__3_1_1_create_response
        runIt requestMessage kmip_1_0__3_1_1_destroy_request
        runIt responseMessage kmip_1_0__3_1_1_destroy_response

      it "3.1.2" $ do
        runIt requestMessage kmip_1_0__3_1_2_register_request
        runIt responseMessage kmip_1_0__3_1_2_register_response
        runIt requestMessage kmip_1_0__3_1_2_create_request
        runIt responseMessage kmip_1_0__3_1_2_create_response
        runIt requestMessage kmip_1_0__3_1_2_get_attributes_request
        runIt responseMessage kmip_1_0__3_1_2_get_attributes_response
        runIt requestMessage kmip_1_0__3_1_2_destroy_request1
        runIt responseMessage kmip_1_0__3_1_2_destroy_response1
        runIt requestMessage kmip_1_0__3_1_2_destroy_request2
        runIt responseMessage kmip_1_0__3_1_2_destroy_response2

      it "3.1.3" $ do
        runIt requestMessage kmip_1_0__3_1_3_create_request
        runIt responseMessage kmip_1_0__3_1_3_create_response
        runIt requestMessage kmip_1_0__3_1_3_locate_request1
        runIt responseMessage kmip_1_0__3_1_3_locate_response1
        runIt requestMessage kmip_1_0__3_1_3_get_request
        runIt responseMessage kmip_1_0__3_1_3_get_response
        runIt requestMessage kmip_1_0__3_1_3_destroy_request
        runIt responseMessage kmip_1_0__3_1_3_destroy_response
        runIt requestMessage kmip_1_0__3_1_3_locate_request2
        runIt responseMessage kmip_1_0__3_1_3_locate_response2

      it "3.1.4" $ do
        runIt requestMessage kmip_1_0__3_1_4_a_register_request
        runIt responseMessage kmip_1_0__3_1_4_a_register_response
        runIt requestMessage kmip_1_0__3_1_4_a_create_request
        runIt responseMessage kmip_1_0__3_1_4_a_create_response
        runIt requestMessage kmip_1_0__3_1_4_b_locate_request
        runIt responseMessage kmip_1_0__3_1_4_b_locate_response
        runIt requestMessage kmip_1_0__3_1_4_b_get_attribute_list_request
        runIt responseMessage kmip_1_0__3_1_4_b_get_attribute_list_response
        runIt requestMessage kmip_1_0__3_1_4_b_get_attributes_request
        runIt responseMessage kmip_1_0__3_1_4_b_get_attributes_response
        runIt requestMessage kmip_1_0__3_1_4_b_add_attribute_request
        runIt responseMessage kmip_1_0__3_1_4_b_add_attribute_response
        runIt requestMessage kmip_1_0__3_1_4_b_modify_attribute_request
        runIt responseMessage kmip_1_0__3_1_4_b_modify_attribute_response
        runIt requestMessage kmip_1_0__3_1_4_b_delete_attribute_request
        runIt responseMessage kmip_1_0__3_1_4_b_delete_attribute_response
        runIt requestMessage kmip_1_0__3_1_4_a_destroy_request1
        runIt responseMessage kmip_1_0__3_1_4_a_destroy_response1
        runIt requestMessage kmip_1_0__3_1_4_a_get_request1
        runIt responseMessage kmip_1_0__3_1_4_a_get_response1
        runIt requestMessage kmip_1_0__3_1_4_a_destroy_request2
        runIt responseMessage kmip_1_0__3_1_4_a_destroy_response2
        runIt requestMessage kmip_1_0__3_1_4_a_get_request2
        runIt responseMessage kmip_1_0__3_1_4_a_get_response2

      it "3.1.5" $ do
        runIt requestMessage kmip_1_0__3_1_5_register_request
        runIt responseMessage kmip_1_0__3_1_5_register_response
        runIt requestMessage kmip_1_0__3_1_5_destroy_request
        runIt responseMessage kmip_1_0__3_1_5_destroy_response

      it "4.1" $ do
        runIt requestMessage kmip_1_0_4_1_a_create_request
        runIt responseMessage kmip_1_0_4_1_a_create_response
        runIt requestMessage kmip_1_0_4_1_a_get_attribute_request
        runIt responseMessage kmip_1_0_4_1_a_get_attribute_response
        runIt requestMessage kmip_1_0_4_1_a_activate_request
        runIt responseMessage kmip_1_0_4_1_a_activate_response
        runIt requestMessage kmip_1_0_4_1_a_get_attribute_request2
        runIt responseMessage kmip_1_0_4_1_a_get_attribute_response2
        runIt requestMessage kmip_1_0_4_1_b_locate_request
        runIt responseMessage kmip_1_0_4_1_b_locate_response
        runIt requestMessage kmip_1_0_4_1_b_get_request
        runIt responseMessage kmip_1_0_4_1_b_get_response
        runIt requestMessage kmip_1_0_4_1_b_revoke_request
        runIt responseMessage kmip_1_0_4_1_b_revoke_response
        runIt requestMessage kmip_1_0_4_1_b_get_attribute_request
        runIt responseMessage kmip_1_0_4_1_b_get_attribute_response
        runIt requestMessage kmip_1_0_4_1_a_get_attribute_list_request
        runIt responseMessage kmip_1_0_4_1_a_get_attribute_list_response
        runIt requestMessage kmip_1_0_4_1_a_get_attributes_request
        runIt responseMessage kmip_1_0_4_1_a_get_attributes_response
        runIt requestMessage kmip_1_0_4_1_a_add_attribute_request
        runIt responseMessage kmip_1_0_4_1_a_add_attribute_response
        runIt requestMessage kmip_1_0_4_1_a_modify_attribute_request
        runIt responseMessage kmip_1_0_4_1_a_modify_attribute_response
        runIt requestMessage kmip_1_0_4_1_a_delete_attribute_request
        runIt responseMessage kmip_1_0_4_1_a_delete_attribute_response
        runIt requestMessage kmip_1_0_4_1_a_get_request
        runIt responseMessage kmip_1_0_4_1_a_get_response
        runIt requestMessage kmip_1_0_4_1_a_destroy_request
        runIt responseMessage kmip_1_0_4_1_a_destroy_response
