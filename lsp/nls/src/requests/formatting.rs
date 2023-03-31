use lsp_server::{RequestId, ResponseError};
use lsp_types::DocumentFormattingParams;

use crate::server::Server;

pub fn handle_format_document(
    params: DocumentFormattingParams,
    id: RequestId,
    server: &mut Server,
) -> Result<(), ResponseError> {
    Ok(())
}
