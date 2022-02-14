//! エラーに関するファイル


use std::num::ParseIntError;

use thiserror::Error;

/// コンパイラ用のエラー型
#[derive(Debug, Error)]
pub enum CompilerError {
    // 実行時引数の数によるエラー
    #[error("Number of Arguments(`{0}`) is not collect")]
    ArgError(usize),
    
    // ファイルへの書き込みエラー
    #[error("IoERROR(`{0}`")]
    IoERROR(std::io::Error),
    
    // char型を数字にパースする場合のエラー
    #[error("ParseIntError(`{0}`")]
    ParseError(ParseIntError),

    // コンパイラのトークンとしておかしい場合のエラー
    #[error("`{0}` is not compilerable string")]
    NotACompilerbleCharError(CharError),

    // 文法エラー
    #[error("Syntax Error")]
    SyntaxError
}

impl From<std::io::Error> for CompilerError {
    fn from(error: std::io::Error) -> CompilerError {
        CompilerError::IoERROR(error)
    }
}

impl From<std::num::ParseIntError> for CompilerError {
    fn from(error: std::num::ParseIntError) -> CompilerError {
        CompilerError::ParseError(error)
    }
}

impl From<CharError> for CompilerError {
    fn from(error: CharError) -> CompilerError {
        CompilerError::NotACompilerbleCharError(error)
    }
}

#[derive(Debug)]
pub struct CharError {
    char: char
}

impl std::fmt::Display for CharError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} is not compilerable", &self.char)
    }
}

impl std::error::Error for CharError {}
