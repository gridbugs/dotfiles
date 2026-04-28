local digit_block_ns = vim.api.nvim_create_namespace("digit_blocks")
vim.api.nvim_set_hl(0, "DigitBlock", { fg = "#FFFFFF" })

local function highlight_digit_blocks_in_number(buf, line_index, number_start, number_end, block_size)
    local num_digits = number_end - number_start
    local num_blocks = ((num_digits - 1) / block_size) + 1
    for block_index = 1, num_blocks do
        local block_end = number_end - 1 - (block_size * (block_index - 1))
        local block_start = math.max(number_start - 1, block_end - block_size)
        local hl_group
        if block_index % 2 == 0 then
            hl_group = "DigitBlock"
        else
            hl_group = "Normal"
        end
        vim.api.nvim_buf_set_extmark(buf, digit_block_ns, line_index - 1, block_start, {
            end_col = block_end,
            hl_group = hl_group,
        })
    end
end

local function highlight_digit_blocks(buf)
    vim.api.nvim_buf_clear_namespace(buf, digit_block_ns, 0, -1)
    local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
    for line_index, line in ipairs(lines) do
        for hex_start, hex_end in line:gmatch("0x()%x+()") do
            highlight_digit_blocks_in_number(buf, line_index, hex_start, hex_end, 4)
        end
        for dec_start, dec_end in line:gmatch("()%d+()") do
            local skip = false
            if dec_start > 2 then
                local prefix = line:sub(dec_start - 2, dec_start - 1)
                if prefix:match('0[xXoObB]') then
                    skip = true
                end
            end
            if not skip then
                highlight_digit_blocks_in_number(buf, line_index, dec_start, dec_end, 3)
            end
        end
    end
end

vim.api.nvim_create_autocmd({ 'BufEnter', 'BufWinEnter', 'TextChanged', 'TextChangedI' }, {
  callback = function(ev) highlight_digit_blocks(ev.buf) end,
})
