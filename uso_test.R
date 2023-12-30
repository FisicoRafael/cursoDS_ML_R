library(testthat)

# Função que vamos testar
add_two_numbers <- function(x, y) {
  return(x + y)
}

# Teste
descricao <- "Teste de adição: "
test_that(descricao, {
  cat(descricao)
  expect_equal(add_two_numbers(2, 3), 5)
})
