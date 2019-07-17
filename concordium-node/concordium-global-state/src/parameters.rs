// https://gitlab.com/Concordium/consensus/globalstate-mockup/blob/master/globalstate/src/Concordium/GlobalState/Parameters.hs

use byteorder::{ByteOrder, NetworkEndian, WriteBytesExt};

use failure::Fallible;

use std::{
    io::{Cursor, Read, Write},
    mem::size_of,
};

use crate::{block::BakerId, common::*};

pub type BakerSignVerifyKey = ByteString;
pub type BakerSignPrivateKey = Encoded;
pub type BakerElectionVerifyKey = Encoded;
pub type BakerElectionPrivateKey = Encoded;
pub type LotteryPower = f64;
pub type ElectionDifficulty = f64;

pub type VoterId = u64;
pub type VoterVerificationKey = ByteString;
pub type VoterVRFPublicKey = Encoded;
pub type VoterSignKey = Encoded;
pub type VoterPower = u64;

pub const BAKER_VRF_KEY: u8 = 32;
const BAKER_SIGN_KEY: u8 = 2 + 32;
const BAKER_INFO: u8 = BAKER_VRF_KEY
    + BAKER_SIGN_KEY
    + size_of::<LotteryPower>() as u8
    + size_of::<AccountAddress>() as u8;

const VOTER_SIGN_KEY: u8 = 2 + 32;
const VOTER_VRF_KEY: u8 = 32;
pub const VOTER_INFO: u8 = VOTER_SIGN_KEY + VOTER_VRF_KEY + size_of::<VoterPower>() as u8;

#[derive(Debug)]
pub struct BirkParameters {
    election_nonce:      ByteString,
    election_difficulty: ElectionDifficulty,
    pub bakers:          Box<[(BakerId, BakerInfo)]>,
    baker_total_stake:   Amount,
    next_baker_id:       BakerId,
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for BirkParameters {
    type Source = &'a mut Cursor<&'b [u8]>;

    fn deserialize(cursor: Self::Source) -> Fallible<Self> {
        let election_nonce = read_bytestring(cursor, "election nonce")?;
        let election_difficulty = NetworkEndian::read_f64(&read_ty!(cursor, ElectionDifficulty));

        let bakers = read_multiple!(
            cursor,
            "bakers",
            (
                NetworkEndian::read_u64(&read_ty!(cursor, BakerId)),
                BakerInfo::deserialize(&read_const_sized!(cursor, BAKER_INFO))?
            ),
            8
        );

        let baker_total_stake = NetworkEndian::read_u64(&read_ty!(cursor, Amount));
        let next_baker_id = NetworkEndian::read_u64(&read_ty!(cursor, BakerId));

        let params = BirkParameters {
            election_nonce,
            election_difficulty,
            bakers,
            baker_total_stake,
            next_baker_id,
        };

        Ok(params)
    }

    fn serialize(&self) -> Box<[u8]> {
        let baker_info_size = 8 + self.bakers.len() * (size_of::<BakerId>() + BAKER_INFO as usize);
        let mut baker_cursor = create_serialization_cursor(baker_info_size);

        let _ = baker_cursor.write_u64::<NetworkEndian>(self.bakers.len() as u64);
        for (id, info) in self.bakers.iter() {
            let _ = baker_cursor.write_u64::<NetworkEndian>(*id);
            let _ = baker_cursor.write_all(&info.serialize());
        }

        debug_assert_eq!(baker_cursor.position(), baker_cursor.get_ref().len() as u64);

        let size = size_of::<u64>()
            + self.election_nonce.len()
            + size_of::<ElectionDifficulty>()
            + baker_cursor.get_ref().len()
            + size_of::<Amount>()
            + size_of::<BakerId>();
        let mut cursor = create_serialization_cursor(size);

        write_bytestring(&mut cursor, &self.election_nonce);
        let _ = cursor.write_f64::<NetworkEndian>(self.election_difficulty);
        let _ = cursor.write_all(baker_cursor.get_ref());
        let _ = cursor.write_u64::<NetworkEndian>(self.baker_total_stake);
        let _ = cursor.write_u64::<NetworkEndian>(self.next_baker_id);

        cursor.into_inner()
    }
}

#[derive(Debug)]
pub struct CryptographicParameters {
    pub elgamal_generator:        ByteString,
    pub attribute_commitment_key: ByteString,
}

impl<'a, 'b: 'a> SerializeToBytes<'a, 'b> for CryptographicParameters {
    type Source = &'a mut Cursor<&'b [u8]>;

    fn deserialize(mut cursor: Self::Source) -> Fallible<Self> {
        let initial_pos = cursor.position() as usize;

        let elgamal_generator = read_bytestring(&mut cursor, "elgamal generator")?;
        let attribute_commitment_key = read_bytestring(&mut cursor, "attribute commitment key")?;

        let crypto_params = CryptographicParameters {
            elgamal_generator,
            attribute_commitment_key,
        };

        let final_pos = cursor.position() as usize;

        check_partial_serialization!(crypto_params, &cursor.get_ref()[initial_pos..final_pos]);

        Ok(crypto_params)
    }

    fn serialize(&self) -> Box<[u8]> {
        let mut cursor = create_serialization_cursor(
            size_of::<u64>()
                + self.elgamal_generator.len()
                + size_of::<u64>()
                + self.attribute_commitment_key.len() as usize,
        );

        write_bytestring(&mut cursor, &self.elgamal_generator);
        write_bytestring(&mut cursor, &self.attribute_commitment_key);

        cursor.into_inner()
    }
}

#[derive(Debug)]
pub struct BakerInfo {
    election_verify_key:  BakerElectionVerifyKey,
    signature_verify_key: BakerSignVerifyKey,
    lottery_power:        LotteryPower,
    account_address:      AccountAddress,
}

impl<'a, 'b> SerializeToBytes<'a, 'b> for BakerInfo {
    type Source = &'a [u8];

    fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let election_verify_key = Encoded::new(&read_const_sized!(&mut cursor, BAKER_VRF_KEY));
        let signature_verify_key =
            read_bytestring_short_length(&mut cursor, "baker sign verify key")?;
        let lottery_power = NetworkEndian::read_f64(&read_ty!(cursor, LotteryPower));
        let account_address = AccountAddress(read_ty!(cursor, AccountAddress));

        let info = BakerInfo {
            election_verify_key,
            signature_verify_key,
            lottery_power,
            account_address,
        };

        check_serialization!(info, cursor);

        Ok(info)
    }

    fn serialize(&self) -> Box<[u8]> {
        let mut cursor = create_serialization_cursor(BAKER_INFO as usize);

        let _ = cursor.write_all(&self.election_verify_key);
        write_bytestring_short_length(&mut cursor, &self.signature_verify_key);
        let _ = cursor.write_f64::<NetworkEndian>(self.lottery_power);
        let _ = cursor.write_all(&self.account_address.0);

        cursor.into_inner()
    }
}

#[derive(Debug)]
pub struct VoterInfo {
    pub signature_verify_key: VoterVerificationKey,
    election_verify_key:      VoterVRFPublicKey,
    voting_power:             VoterPower,
}

impl<'a, 'b> SerializeToBytes<'a, 'b> for VoterInfo {
    type Source = &'a [u8];

    fn deserialize(bytes: &[u8]) -> Fallible<Self> {
        let mut cursor = Cursor::new(bytes);

        let signature_verify_key =
            read_bytestring_short_length(&mut cursor, "signature verify key")?;
        let election_verify_key = Encoded::new(&read_const_sized!(&mut cursor, VOTER_VRF_KEY));
        let voting_power = NetworkEndian::read_u64(&read_ty!(cursor, VoterPower));

        let info = VoterInfo {
            signature_verify_key,
            election_verify_key,
            voting_power,
        };

        check_serialization!(info, cursor);

        Ok(info)
    }

    fn serialize(&self) -> Box<[u8]> {
        let mut cursor = create_serialization_cursor(VOTER_INFO as usize);

        write_bytestring_short_length(&mut cursor, &self.signature_verify_key);
        let _ = cursor.write_all(&self.election_verify_key);
        let _ = cursor.write_u64::<NetworkEndian>(self.voting_power);

        cursor.into_inner()
    }
}
